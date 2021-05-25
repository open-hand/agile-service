import React, { useEffect, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { Icon } from 'choerodon-ui';
import { stores } from '@choerodon/boot';
import { Issue, IIssueType } from '@/common/types';
import { DataSet } from 'choerodon-ui/pro';
import styles from './Confirm.less';
import store, { FieldWithValue } from '../../store';
import IssueCard from '../issue-card';

export interface IssueWithSubIssueVOList extends Omit<Issue, 'subIssueVOList'> {
  subIssueVOList: Issue[]
}

export interface ILoseItems {
  test: boolean,
  doc: boolean,
  backlog: boolean,
  linkIssue: boolean,
}
interface Props {
  issue: any,
  dataSet: DataSet,
  fieldsWithValue: FieldWithValue[]
  targetProjectType: 'program' | 'project' | 'subProject'
  targetIssueType?: IIssueType
  targetSubTaskType?: IIssueType
  targetSubBugType?: IIssueType
  loseItems: ILoseItems,
}

const Confirm: React.FC<Props> = ({
  issue: mainIssue, dataSet, fieldsWithValue, targetProjectType, targetIssueType, targetSubTaskType, targetSubBugType, loseItems,
}) => {
  const {
    dataMap, selfFields, subTaskFields, subBugFields, moveToProjectList, subTaskDetailMap, subBugDetailMap, subTaskTypeId, subBugTypeId, selectedUserIds, selectedUsers,
  } = store;
  const targetProjectId = dataSet?.current?.get('targetProjectId');
  const issueType = dataSet?.current?.get('issueType');
  const { subIssueVOList, subBugVOList } = mainIssue;

  // const targetIssueTypeIds = [issueType, subTaskTypeId, subBugTypeId].filter(Boolean);
  const targetIssueTypes = useMemo(() => [targetIssueType, targetSubTaskType, targetSubBugType].filter(Boolean), [targetIssueType, targetSubBugType, targetSubTaskType]);

  const { issues, issueFields } = store;
  useEffect(() => {
    store.initIssueMap(issueType, mainIssue);
    store.loadData(targetIssueTypes as IIssueType[], targetProjectId, targetProjectType);
  }, [issueType, mainIssue, targetIssueTypes, targetProjectId, targetProjectType]);
  return (
    <div className={styles.confirm}>
      <div className={styles.tip}>
        <Icon type="report" />
        <p className={styles.tipText}>
          {/* {tipText} */}
        </p>
      </div>
      <div className={styles.content}>
        <div className={styles.contentTip}>
          系统将保留兼容的字段值，您可以根据需要更新以下不兼容的字段值：
        </div>
        <div className={styles.contentMain}>
          {
            issues.map((issue: Issue, index) => (
              <IssueCard
                sourceIssue={issue}
                sourceFields={issueFields[index]}
                key={issue.issueId}
              />
            ))
          }
        </div>
      </div>
    </div>
  );
};

export default observer(Confirm);
