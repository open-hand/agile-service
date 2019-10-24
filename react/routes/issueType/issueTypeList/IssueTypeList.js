import React, {
  useState, useEffect, useContext,
} from 'react';
import { observer } from 'mobx-react-lite';
import { withRouter } from 'react-router-dom';
import {
  Table, Button, Modal, message, Icon,
} from 'choerodon-ui';
import { FormattedMessage } from 'react-intl';
import {
  Content, TabPage as Page, Breadcrumb,
} from '@choerodon/boot';

import './IssueTypeList.less';
import IssueTypeCreate from '../issueTypeCreate';
import TypeTag from '../../../components/TypeTag/TypeTag';
import Store from '../stores';


/**
 * 函数组件 
 * 现在废弃编辑，创建，删除。
 * 
 * 鼠标点击相关方案，出现弹窗是否进入关联的相关方案 待定
 */
function IssueTypeList(props) {
  const context = useContext(Store);
  const { AppState, issueTypeStore, intl } = context;
  const [issueTypeId, setIssueTypeId] = useState('');
  const [visible, setVisible] = useState(false);
  const [deleteVisible, setDeleteVisible] = useState(false);
  const [issueType, setIssueType] = useState(false);
  const [tableParam, setTableParam] = useState({
    sorter: undefined,
    param: {},
    page: 1,
    pageSize: 10,
  });
  const [submitting, setSubmitting] = useState(false);

  const modelRef = false;

  const linkToScheme = (schemeId) => {
    modelRef.destroy();
    const { history } = context;
    const { name, id, organizationId } = AppState.currentMenuType;
    // 方案详情目前不可访问
    history.push(`/agile/issue-type-schemes?type=organization&id=${id}&name=${encodeURIComponent(name)}&organizationId=${organizationId}&orgId=${organizationId}`);
  };


  // 有问题， this.state;
  const loadIssueType = () => {
    const orgId = AppState.currentMenuType.organizationId;
    issueTypeStore.loadIssueType(orgId, tableParam.page ? tableParam.page - 1 : undefined, tableParam.pageSize, tableParam.sorter, tableParam.param);
  };

  const hideSidebar = () => {
    setIssueTypeId(false);
    issueTypeStore.setCreateTypeShow(false);
    loadIssueType();
  };

  const closeRemove = () => {
    setDeleteVisible(false);
    setVisible(false);
    setIssueTypeId(false);
    setIssueType(false);
  };

  const handleDelete = () => {
    const orgId = AppState.currentMenuType.organizationId;
    issueTypeStore.deleteIssueType(orgId, issueTypeId)
      .then((data) => {
        if (data) {
          message.success(intl.formatMessage({ id: 'deleteSuccess' }));
        } else {
          message.error(intl.formatMessage({ id: 'deleteFailed' }));
        }
        closeRemove();
        loadIssueType();
      }).catch((error) => {
        message.error(intl.formatMessage({ id: 'deleteFailed' }));
        closeRemove();
      });
  };

  const handleTableChange = (pagination, filters, sorter, param) => {
    const sort = {};
    if (sorter.column) {
      const { field, order } = sorter;
      sort[field] = order;
    }
    let searchParam = {};
    if (filters && filters.name && filters.name.length) {
      searchParam = {
        ...searchParam,
        name: filters.name[0],
      };
    }
    if (filters && filters.description && filters.description.length) {
      searchParam = {
        ...searchParam,
        description: filters.description[0],
      };
    }
    if (param && param.length) {
      searchParam = {
        ...searchParam,
        param: param.toString(),
      };
    }
    setTableParam({
      sorter: sorter.column ? sorter : undefined,
      param: searchParam,
      page: pagination.current,
      pageSize: pagination.pageSize,
    });
  };

  const getColumn = () => ([{
    title: <FormattedMessage id="issueType.name" />,
    dataIndex: 'name',
    key: 'name',
    filters: [],
    render: (text, record) => (
      <TypeTag
        data={record}
        showName
        style={{ margin: 0 }}
      />
    ),
  }, {
    title: <FormattedMessage id="issueType.des" />,
    dataIndex: 'description',
    key: 'description',
    filters: [],
    className: 'issue-table-ellipsis',
  }]);


  useEffect(() => {
    loadIssueType();
  }, [tableParam]);

  function render() {
    return (
      <Page className="issue-region">
        <Breadcrumb />
        <Content>
          <Table
            dataSource={issueTypeStore.getIssueTypes}
            columns={getColumn()}
            loading={issueTypeStore.getIsLoading}
            rowKey={record => record.id}
            pagination={issueTypeStore.pageInfo}
            onChange={handleTableChange}
            filterBarPlaceholder="过滤表"
            className="issue-table"
          />
        </Content>
        {issueTypeStore.createTypeShow && (
          <IssueTypeCreate
            id={issueTypeId}
            store={issueTypeStore}
            visible={!!issueTypeStore.createTypeShow}
            onClose={hideSidebar}
          />
        )}
        <Modal
          confirmLoading={submitting}
          visible={deleteVisible}
          title={<FormattedMessage id="issueType.action.delete" />}
          closable={false}
          footer={[
            <Button key="back" onClick={closeRemove}><FormattedMessage id="cancel" /></Button>,
            <Button key="submit" type="danger" onClick={handleDelete} loading={submitting}>
              {intl.formatMessage({ id: 'delete' })}
            </Button>,
          ]}
        >
          <p className="issue-issueType-tip">
            {intl.formatMessage({ id: 'issueType.delete' })}
            <span className="issue-issueType-bold">{issueType.name}</span>
          </p>
          <p className="issue-issueType-tip">
            {intl.formatMessage({ id: 'issueType.delete.confirm' })}
          </p>
          <p className="issue-issueType-tip">
            {intl.formatMessage({ id: 'issueType.delete.noUse' })}
          </p>
          <p className="issue-issueType-tip">
            {intl.formatMessage({ id: 'issueType.delete.noUseTip' })}
          </p>
        </Modal>
        <Modal
          visible={visible}
          title={<FormattedMessage id="issueType.action.delete" />}
          closable={false}
          footer={[
            <Button key="back" onClick={closeRemove}><FormattedMessage id="cancel" /></Button>,
          ]}
        >
          <p className="issue-issueType-tip">
            {intl.formatMessage({ id: 'issueType.delete' })}
            <span className="issue-issueType-bold">{issueType.name}</span>
          </p>
          <p className="issue-issueType-tip">
            <Icon type="error" className="issue-issueType-icon issue-error-msg" />
            {intl.formatMessage({ id: 'issueType.delete.forbidden' })}
          </p>
          <p className="issue-issueType-tip">
            <FormattedMessage
              id="issueType.delete.inUse"
              values={{
                num: 1,
              }}
            />
          </p>
          <p className="issue-issueType-tip">
            {intl.formatMessage({ id: 'issueType.delete.inUseTip' })}
          </p>
        </Modal>
      </Page>
    );
  }
  return render();
}

export default withRouter(observer(IssueTypeList));
