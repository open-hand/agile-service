import React from 'react';
import { observer } from 'mobx-react-lite';
import { DataSet } from 'choerodon-ui/pro';
import { IField, IIssueType } from '@/common/types';
import RequiredField from '@/components/required-field';
import { TypeTag } from '@/components';
import { RequiredFieldDs } from '@/components/required-field/useRequiredFieldDataSet';
import styles from './SubTaskRequired.less';

export interface ISubTaskRequiredItem {
  issueId: string, issueTypeId?: string, issueTypeVO: IIssueType, issueNum: string, summary: string, requiredFields: IField[],
}

interface Props {
  item: ISubTaskRequiredItem
  requiredFieldDsArr: RequiredFieldDs[]
}

const SubTaskRequired: React.FC<Props> = ({ item, requiredFieldDsArr }) => {
  const {
    issueId, issueTypeVO, issueNum, summary, requiredFields,
  } = item;
  return (
    <div className={styles.subTaskRequired} key={issueId}>
      <div className={styles.subTaskRequired_info}>
        <TypeTag
          data={issueTypeVO}
        />
        <span style={{ marginLeft: '2px', marginRight: '8px' }}>{issueNum}</span>
        <span>{summary}</span>
      </div>
      <div>
        <RequiredField
          requiredFields={requiredFields}
          requiredFieldDataSet={requiredFieldDsArr.find((issueDs) => issueDs.issueId === issueId)?.dataSet as DataSet}
        />
      </div>
    </div>
  );
};

export default observer(SubTaskRequired);
