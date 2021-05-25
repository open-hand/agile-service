import React, {
  useEffect, useState, useCallback, useMemo,
} from 'react';
import { observer, useLocalStore } from 'mobx-react-lite';
import {
  Icon, Row, Col,
} from 'choerodon-ui';
import { stores } from '@choerodon/boot';
import {
  Issue, IField, IIssueType,
} from '@/common/types';
import {
  includes, uniq, compact, flatten, find, findIndex,
} from 'lodash';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import DataSetField from 'choerodon-ui/pro/lib/data-set/Field';
import TypeTag from '@/components/TypeTag';
import { DataSet, Tooltip } from 'choerodon-ui/pro';
import {
  fieldApi, moveIssueApi, issueApi, userApi,
} from '@/api';
import styles from './Confirm.less';
import transformValue, { IFieldWithValue } from './transformValue';
import store from '../../store';
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
  fieldsWithValue: IFieldWithValue[]
  targetProjectType: 'program' | 'project' | 'subProject'
  targetIssueType?: IIssueType
  targetSubTaskType?: IIssueType
  targetSubBugType?: IIssueType
  loseItems: ILoseItems,
}

const { AppState } = stores;

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

  const { issues, issueMap } = store;
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
            issues.map((issue: Issue) => (
              <IssueCard
                sourceIssue={issue}
              />
            ))
          }
        </div>
      </div>
    </div>
  );
};

export default observer(Confirm);
