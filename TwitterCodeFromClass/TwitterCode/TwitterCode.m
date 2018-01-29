%  Tutorial from
%  https://blogs.mathworks.com/loren/2014/06/04/analyzing-twitter-with-matlab/

%DO NOT GIVE OUT THE FOLLOWING INFORMATION TO ANYONE
credentials.ConsumerKey = '3bxyiuG3DJzmXlUvAF6NRQpv5';
credentials.ConsumerSecret = 'i3Td2j5iQToDCcSTm0kLshb6vIIETutoT2YHYY40kbPt9oqr4V';
credentials.AccessToken = '953818124243087360-kEijMPCzceyjWH3D9Hf8BSImq9K3ihp';
credentials.AccessTokenSecret = 'GjDCLKyT1klUeRVJoEsSH74z7tFt8fEc5cj9Ks3YYp6vs';

% set up a Twitty object
addpath twitty_1.1.1; % Twitty
%addpath parse_json; % Twitty's default json parser
addpath jsonlab; % I prefer JSONlab, however.
tw = twitty(credentials); % instantiate a Twitty object
tw.jsonParser = @loadjson; % specify JSONlab as json parser


%% This code is set up to pull the last numTweets with the specified word
lang='en';
include_entities='true';
numTweets=105;
result_type='recent';


% search for English tweets that mention several words. 
% This will return a cell array containing the query results for each of
% the words you request
words={'NFL','Eagles','Patriots'};

%words={'NFL'};

if numTweets<=100
    tweetPerSearch=numTweets;
    twitSearch=cell(1,length(words));
    for i=1:length(words)
        word=words{i}
        twitSearch(i) = tw.search(word,'count',tweetPerSearch,'include_entities', include_entities,....
                                  'lang',lang,'result_type',result_type);
    end
else
    numIter=ceil(numTweets/100);
    twitSearch=cell(1,length(words));
    for i=1:length(words)
        twitSearch{1,i}.statuses={};
        word=words{i}
        max_id=inf;
        for iter=1:numIter
            fprintf('Iteration %d of %d total.\n',iter,numIter);
            %Calculate number of tweets to get on current iteration
            if(iter<numIter)
                tweetPerSearch=100;
            else%last iteration get the remaining bit of tweets
                tweetPerSearch=mod(numTweets,100);
            end
            tempSearch(i) = tw.search(word,'count',tweetPerSearch,'include_entities', include_entities,....
                'lang',lang,'result_type',result_type,'max_id',max_id);
            twitSearch{1,i}.statuses=horzcat(twitSearch{1,i}.statuses,tempSearch{1,i}.statuses)
            twitSearch{1,i}.search_metadata=tempSearch{1,i}.search_metadata
            max_id=twitSearch{1,i}.statuses{1,length(twitSearch{1,i}.statuses)}.id_str;
        end

    end
    
end

%% We can also find all tweets with multiple words in them.
% multWords = tw.search('NFL Patriots','count',100,'include_entities','true','lang','en');
% [Users,Tweets] = processTweets.extract(multWords);
%     



% load supporting data for text processing
scoreFile = 'AFINN/AFINN-111.txt';
stopwordsURL ='http://www.textfixer.com/resources/common-english-words.txt';
% load previously saved data
%load amazonHachette.mat
userTweetsCell=cell(length(twitSearch),2);
for i=1:length(twitSearch)
    % process the structure array with a utility method |extract|
    [userTweetsCell{i,1},userTweetsCell{i,2}] = processTweets.extract({twitSearch{i}});
    
end

%Extract user/tweets tables for each of the words used above
NFL.users=userTweetsCell{1,1};
NFL.tweets=userTweetsCell{1,2};
Eagles.users=userTweetsCell{2,1};
Eagles.tweets=userTweetsCell{2,2};
Patriots.users=userTweetsCell{3,1};
Patriots.tweets=userTweetsCell{3,2};



% compute the sentiment scores with |scoreSentiment|
NFLSentiment = processTweets.scoreSentiment(NFL.tweets, scoreFile,stopwordsURL);
EaglesSentiment = processTweets.scoreSentiment(Eagles.tweets, scoreFile,stopwordsURL);
PatriotsSentiment = processTweets.scoreSentiment(Patriots.tweets, scoreFile,stopwordsURL);

% calculate and print NSRs
NFL_NSR = ( sum(NFLSentiment>=0) - sum(NFLSentiment<0) ) / height(NFL.tweets);
Eagles_NSR = ( sum(EaglesSentiment>=0) - sum(EaglesSentiment<0) ) / height(Eagles.tweets);
Patirots_NSR = ( sum(PatriotsSentiment>=0) - sum(PatriotsSentiment<0) ) / height(Patriots.tweets);

fprintf('NFL NSR  :  %.2f\n',NFL_NSR)
fprintf('Eagles NSR  :  %.2f\n',Eagles_NSR)
fprintf('Pats NSR  :  %.2f\n',Patirots_NSR)

% plot the sentiment histogram of two brands
binranges = min([NFLSentiment; ...
    EaglesSentiment; ...
    PatriotsSentiment]): ...
    max([NFLSentiment; ...
    EaglesSentiment; ...
    PatriotsSentiment]);
bincounts = [histc(NFLSentiment,binranges)...
    histc(EaglesSentiment,binranges)...
    histc(PatriotsSentiment,binranges)];
figure
bar(binranges,bincounts,'hist')
legend('NFL','Eagles','Patriots','Location','Best')
title('Sentiment Distribution of 100 Tweets')
xlabel('Sentiment Score')
ylabel('# Tweets')

              
% tokenize tweets with |tokenize| method of |processTweets|
[commWords, dict] = processTweets.tokenize(NFL.tweets,stopwordsURL);
% create a dictionary of unique words
dict = unique(dict);
% create a word count matrix
[~,tdf] = processTweets.getTFIDF(commWords,dict);

% plot the word count
figure
plot(1:length(dict),sum(tdf),'b.')
xlabel('Word Indices')
ylabel('Word Count')
title('Words contained in the tweets')
% annotate high frequency words
annotated = find(sum(tdf)>= 10);
jitter = 6*rand(1,length(annotated))-3;
for i = 1:length(annotated)
    text(annotated(i)+3, ...
        sum(tdf(:,annotated(i)))+jitter(i),dict{annotated(i)})
end









% go here to find a specific woeid:
% http://woeid.rosselliot.co.nz/
% world_woeid='1'
usa_woeid = '23424977'; % USA
usa_trends = tw.trendsPlace(usa_woeid);
usa_trends = cellfun(@(x) x.name, usa_trends{1}.trends, 'UniformOutput',false)';








%Live streaming(more than 100 tweets)
tw.outFcn = @saveTweets; % output function
tw.sampleSize = 1000;  % default 1000
tw.batchSize = 1; % default 20
tic;
tw.filterStatuses('track',usa_trends{1}); % Streaming API call
toc
uk_data = tw.data; % save the data

save('uk_data')
% reload the previously saved search result for 4 trending topics in the UK
load('uk_data.mat')


% 
% % plot
% figure
% i=1;
% %for i = 1:4
%     % process tweets
%     [users,tweets] = processTweets.extract(uk_data(i).statuses);
% 
%     % get who are mentioned in retweets
%     retweeted = tweets.Mentions(tweets.isRT);
%     retweeted = retweeted(~cellfun('isempty',retweeted));
%     [screen_names,~,idx] = unique(retweeted);
%     count = accumarray(idx,1);
%     retweeted = table(screen_names,count,'VariableNames',{'Screen_Name','Count'});
% 
%     % get the users who were mentioned in retweets
%     match = ismember(users.Screen_Name,retweeted.Screen_Name);
%     retweetedUsers = sortrows(users(match,:),'Screen_Name');
%     match = ismember(retweeted.Screen_Name,retweetedUsers.Screen_Name);
%     retweetedUsers.Retweeted_Count = retweeted.Count(match);
%     [~,order] = sortrows(retweetedUsers,'Retweeted_Count','descend');
% 
%     % plot each topic
%     subplot(2,2,i)
%     scatter(retweetedUsers.Followers(order),...
%         retweetedUsers.Retweeted_Count(order),retweetedUsers.Freq(order)*50,...
%         retweetedUsers.Freq(order),'fill')
% 
%     if ismember(i, [1,2])
%         ylim([-20,90]); xpos = 2; ypos1 = 50; ypos2 = 40;
%     elseif i == 3
%         ylim([-1,7])
%         xlabel('Follower Count (Log Scale)')
%         xpos = 1010; ypos1 = 0; ypos2 = -1;
%     else
%         ylim([-5,23])
%         xlabel('Follower Count (Log Scale)')
%         xpos = 110; ypos1 = 20; ypos2 = 17;
%     end
% 
%     % set x axis to log scale
%     set(gca, 'XScale', 'log')
% 
%     if ismember(i, [1,3])
%         ylabel('Retweeted Count')
%     end
%     title(sprintf('UK Tweets'))
% %end
