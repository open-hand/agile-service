import React, { useContext } from 'react';
import { observer } from 'mobx-react-lite';
import { withRouter } from 'react-router-dom';
import {
  Table,
} from 'choerodon-ui/pro';
import {
  Content, Page, Breadcrumb,
} from '@choerodon/boot';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { IIssueType } from '@/common/types';
import TypeTag from '../../../components/TypeTag/TypeTag';
import Store from '../stores';
import styles from './IssueTypeList.less';

const { Column } = Table;

/**
 * 问题类型页面
 * 鼠标点击相关方案，出现弹窗是否进入关联的相关方案 待定
 */
function IssueTypeList() {
  const context = useContext(Store);
  const { issueTypeDataSet } = context;

  /**
   * render Name
   * @param {*} param0
   */
  function renderName({ record }: RenderProps) {
    const colour = record?.get('colour');
    const name = record?.get('name');
    const icon = record?.get('icon');
    const data = {
      colour,
      name,
      icon,
    };
    return (
      <TypeTag
        data={data as IIssueType}
        showName
        style={{ margin: 0 }}
      />
    );
  }
  return (
    <Page
      className={styles.issueType}
      service={[
        'choerodon.code.organization.setting.issue.issue-type.ps.default',
      ]}
    >
      <Breadcrumb />
      <Content style={{ paddingTop: '0' }}>
        <Table dataSet={issueTypeDataSet} className={styles.issueTypeTable}>
          <Column name="name" renderer={renderName} />
          <Column name="description" />
        </Table>
      </Content>
    </Page>
  );
}

export default withRouter(observer(IssueTypeList));
