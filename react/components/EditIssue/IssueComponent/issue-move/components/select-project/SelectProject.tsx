import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { Select, Form, DataSet } from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import SelectTargetProject from '@/components/select/select-team';
import TypeTag from '@/components/TypeTag';
import { IIssueType } from '@/common/types';
import { moveIssueApi } from '@/api';
import styles from './SelectProject.less';
import { IssueWithSubIssueVOList } from '../confirm-data/Confirm';
import store from '../../store';

interface Props {
  dataSet: DataSet,
  issueTypeDataSet: DataSet,
  issue: IssueWithSubIssueVOList
}

const SelectProject: React.FC<Props> = ({ issue, dataSet, issueTypeDataSet }) => {
  const hasSubIssue = issue.subIssueVOList?.length > 0;
  const hasSubBug = issue.subBugVOList?.length > 0;

  return (
    <div className={styles.selectProject}>
      <div className={styles.tip}>
        <Icon type="report" />
        <p className={styles.tipText}>
          由于目标项目与源项目的字段设置不同，在不同项目之间移动问题项，您可能会丢失部分数据信息。即使您移回源项目，也无法恢复这些数据。
        </p>
      </div>
      <div className={styles.form}>
        <div className={styles.formTip}>请选择目标项目和问题类型：</div>
        <Form dataSet={dataSet}>
          <SelectTargetProject
            name="targetProjectId"
            textField="name"
            valueField="id"
            request={() => moveIssueApi.getProjectListMoveTo(issue.typeCode as string)}
            afterLoad={(data) => {
              store.setMoveToProjectList(data);
            }}
          />
          <Select
            name="issueType"
            optionsFilter={hasSubBug ? (record) => record.get('typeCode') === 'story' || record.get('typeCode') === 'task'
              : (record) => record.get('typeCode') !== 'sub_task'}
          />
          {hasSubIssue && (
            <Select
              name="subTaskIssueTypeId"
              optionsFilter={(record) => record.get('typeCode') === 'sub_task'}
            />
          )}
          {hasSubBug && (
            <Select
              name="subBugIssueTypeId"
              optionsFilter={(record) => record.get('typeCode') === 'bug'}
            />
          )}
        </Form>
      </div>
    </div>
  );
};

export default observer(SelectProject);
