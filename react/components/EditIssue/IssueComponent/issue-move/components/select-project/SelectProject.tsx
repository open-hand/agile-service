import React from 'react';
import { observer } from 'mobx-react-lite';
import { Select, Form, DataSet } from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import SelectTargetProject from '@/components/select/select-team';
import TypeTag from '@/components/TypeTag';
import { IIssueType } from '@/common/types';
import { moveIssueApi } from '@/api';
import styles from './SelectProject.less';

interface Props {
  dataSet: DataSet,
  issueTypeDataSet: DataSet,
}

const SelectProject: React.FC<Props> = ({ dataSet, issueTypeDataSet }) => (
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
        <SelectTargetProject name="targetProjectId" textField="name" valueField="id" request={() => moveIssueApi.getProjectListMoveTo()} />
        <Select name="issueType">
          {
            issueTypeDataSet.toData().map((issueType: IIssueType) => (
              issueType.typeCode && (
                <TypeTag
                  data={issueType}
                  showName
                />
              )
            ))
          }
        </Select>
      </Form>
    </div>
  </div>
);

export default observer(SelectProject);
