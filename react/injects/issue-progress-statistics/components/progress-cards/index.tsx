import React from 'react';
import IssueProgressCard from '../issue-progress-card';
import styles from './index.less';

interface IssueProgressStatisticsCardProps {

}
const IssueProgressStatisticsCards: React.FC<IssueProgressStatisticsCardProps> = () => {
  React.useEffect(() => { }, []);
  return (
    <div className={styles.cards}>
      <IssueProgressCard type="issue" data={{ total: 10, percentage: '50%', finish: 20 }} className={styles.cards_item} />
      <IssueProgressCard type="workTime" data={{ total: 10, percentage: '50%', finish: 20 }} className={styles.cards_item} />
      <IssueProgressCard type="storyPoint" data={{ total: 10, percentage: '50%', finish: 20 }} className={styles.cards_item} />
      <IssueProgressCard type="nonEnvironmentBug" data={{ total: 10, percentage: '50%', finish: 20 }} className={styles.cards_item} />
      <IssueProgressCard type="environmentBug" data={{ total: 10, percentage: '50%', finish: 20 }} className={styles.cards_item} />
    </div>
  );
};

export default IssueProgressStatisticsCards;
