import React, { useEffect, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { Icon, DataSet } from 'choerodon-ui/pro';
import { Issue, IIssueType } from '@/common/types';

import { OldLoading as Loading } from '@/components/Loading';
import styles from './Confirm.less';
import store, { FieldWithValue } from '../../store';
import IssueCard from '../issue-card';
import LostFields from './LostFields';
import { getProjectId } from '@/utils/common';

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
  projectId?: string
  loseItems: ILoseItems,
}

const Confirm: React.FC<Props> = ({
  issue: mainIssue, dataSet, targetProjectType, targetIssueType, targetSubTaskType, targetSubBugType, loseItems, projectId,
}) => {
  const { moveToProjectList } = store;
  const targetProjectId = dataSet?.current?.get('targetProjectId');
  const issueType = dataSet?.current?.get('issueType');

  const targetIssueTypes = useMemo(() => [targetIssueType, targetSubTaskType, targetSubBugType].filter(Boolean), [targetIssueType, targetSubBugType, targetSubTaskType]);

  const { issues, issueFields, issueMapValues } = store;
  useEffect(() => {
    (async () => {
      store.setLoading(true);
      await store.initIssueMap(issueType, mainIssue);
      await store.loadData(targetIssueTypes as IIssueType[], targetProjectId, targetProjectType, projectId || getProjectId());
      store.setLoading(false);
    })();
  }, [issueType, mainIssue, projectId, targetIssueTypes, targetProjectId, targetProjectType]);
  const targetProject = useMemo(() => moveToProjectList.find((item: any) => item.id === targetProjectId) || { name: '' }, [moveToProjectList, targetProjectId]);
  if (store.loading) {
    return <Loading loading />;
  }

  return (
    <div className={styles.confirm}>
      {
        (issueMapValues.length > 0 && (
          issueMapValues.some((item) => !!item.lostFields?.length) || !!(
            [...Object.entries(loseItems)].filter(([, v]) => !!v).length
          ))) && (
          <div className={styles.tip}>
            <Icon type="report" />
            <p className={styles.tipText}>
              {issueMapValues.map(({ issue, lostFields }) => (
                <LostFields
                  key={issue.issueId}
                  lostFields={lostFields}
                  loseItems={loseItems}
                  targetProject={targetProject}
                  issue={issue}
                />
              ))}
            </p>
          </div>
        )
      }
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
                // 第一个是主issue，其他issue是子任务和子bug，禁用选择冲刺，跟着父级走
                disabledSprint={index > 0}
              />
            ))
          }
        </div>
      </div>
    </div>
  );
};

export default observer(Confirm);
