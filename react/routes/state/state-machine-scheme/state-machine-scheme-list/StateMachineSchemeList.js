import React, {
  Component, Fragment, useState, useEffect, useContext,
} from 'react';
import { observer, inject } from 'mobx-react-lite';
import { withRouter, Link } from 'react-router-dom';
import {
  Table,
  Button,
  Modal,
  Form,
  Select,
  Input,
  Icon,
  Tooltip,
  message,
  Menu,
  Breadcrumb as Bread,
} from 'choerodon-ui';
import { Modal as ProModal } from 'choerodon-ui/pro';
import { injectIntl, FormattedMessage } from 'react-intl';
import {
  Content,
  Header,
  TabPage as Page,
  Permission,
  Breadcrumb,
} from '@choerodon/boot';

import './StateMachineSchemeList.less';
import TypeTag from '../../../../components/TypeTag/TypeTag';
import Store from '../stores';
import TableDropMenu from '../../../../common/TableDropMenu';
import EditStateMachineSchemeName from '../EditStateMachineSchemeName';

const prefixCls = 'issue-stateMachineScheme';
const { Sidebar } = Modal;
const FormItem = Form.Item;
const { TextArea } = Input;
const { Item } = Bread;

const formItemLayout = {
  labelCol: {
    xs: { span: 24 },
    sm: { span: 100 },
  },
  wrapperCol: {
    xs: { span: 24 },
    sm: { span: 26 },
  },
};

function StateMachineSchemeList(props) {
  const context = useContext(Store);
  const { AppState, stateMachineSchemeStore, intl } = context;
  const [show, setShow] = useState(false);
  const [submitting, setSubmitting] = useState(false);
  const [deleteId, setDeleteId] = useState(0);
  const [deleteItemName, setDeleteItemName] = useState('');
  const [showType, setShowType] = useState('');

  const [tableParam, setTableParam] = useState({
    sorter: undefined,
    param: {},
    page: 1,
    pageSize: 10,
  });

  const showSideBar = (state, id = '') => {
    props.form.resetFields();
    setShow(true);
    setShowType(state);
    setSubmitting(false);
  };

  const hideSidebar = () => {
    setShow(false);
    setShowType('');
    setSubmitting(false);
  };
  // 此前未定义
  const closeRemove = () => {
    // 未完成
    // console.log('清理状态 未完成');
  };

  const handleEdit = (record) => {
    const { history } = context;
    const { name, id, organizationId } = AppState.currentMenuType;
    // stateMachineSchemeStore.loadStateMachine(organizationId, id);
    history.push(`/agile/states/scheme/edit/${record.id}?type=organization&id=${id}&name=${encodeURIComponent(name)}&organizationId=${organizationId}&orgId=${organizationId}`);
  };

  const handleDelete = (newDeleteId, newDeleteItemName) => {
    setDeleteId(newDeleteId);
    setDeleteItemName(newDeleteItemName);
    stateMachineSchemeStore.setIsSchemeDeleteVisible(true);
  };


  const cancelDelete = (e) => {
    stateMachineSchemeStore.setIsSchemeDeleteVisible(false);
  };

  const handleSubmit = (e) => {
    const { organizationId } = AppState.currentMenuType;
    e.preventDefault();
    const { form } = props;
    setSubmitting(true);
    form.validateFields((err, stateMachineScheme) => {
      if (!err) {
        stateMachineSchemeStore.createStateMachineScheme(stateMachineScheme, organizationId)
          .then(() => {
            hideSidebar();
          });
      }
    });
  };

  const loadStateMachineSchemeList = (pagination, sort = { field: 'id', order: 'desc' }, param) => {
    const orgId = AppState.currentMenuType.organizationId;
    stateMachineSchemeStore.loadStateMachineSchemeList(orgId, pagination, sort, { ...param });
  };

  const refresh = () => {
    const { pagination } = stateMachineSchemeStore;
    loadStateMachineSchemeList(pagination, tableParam.sorter, tableParam.param);
  };

  const tableChange = (pagination, filters, sorter, param) => {
    const orgId = AppState.currentMenuType.organizationId;
    const sort = {};
    if (sorter.column) {
      const { field, order } = sorter;
      sort[field] = order;
    }
    let searchParam = {};
    if (Object.keys(filters).length) {
      searchParam = filters;
    }
    const postData = {
      ...searchParam,
      param: param.toString(),
    };
    // this.setState({
    //   sorter: sorter.column ? sorter : undefined,
    //   tableParam: postData,
    // });
    setTableParam({
      sorter: sorter.column ? sorter : undefined,
      param: postData,
    });

    loadStateMachineSchemeList(pagination,
      sorter.column ? sorter : undefined,
      postData);
  };

  const confirmDelete = (schemeId) => {
    const { organizationId } = AppState.currentMenuType;
    stateMachineSchemeStore.deleteStateMachineScheme(organizationId, schemeId).then((data) => {
      if (data) {
        message.success(intl.formatMessage({ id: 'deleteSuccess' }));
      } else {
        message.error(intl.formatMessage({ id: 'deleteFailed' }));
      }
      stateMachineSchemeStore.setIsSchemeDeleteVisible(false);
      refresh();
    }).catch((error) => {
      message.error(intl.formatMessage({ id: 'deleteFailed' }));
      closeRemove();
    });
  };


  const checkName = async (rule, value, callback) => {
    const orgId = AppState.currentMenuType.organizationId;
    const res = await stateMachineSchemeStore.checkName(orgId, value);
    if (res) {
      callback(intl.formatMessage({ id: 'priority.create.name.error' }));
    } else {
      callback();
    }
  };
  const handleEditName = (record) => {
    ProModal.open({
      key: 'editName',
      title: '编辑状态机方案',
      drawer: true,
      style: {
        width: 380,
      },
      children: (
        <EditStateMachineSchemeName
          AppState={AppState}
          StateMachineSchemeStore={stateMachineSchemeStore}
          intl={intl}
          record={record}
        />
      ),
      okText: '保存',
    });
  };

  const handleChooseMenu = (key, record) => {
    if (key === 'delete') {
      handleDelete(record.id, record.name);
    } else if (key === 'editName') {
      handleEditName(record);
    }
  };
  const getColumns = () => [
    {
      title: <FormattedMessage id="stateMachineScheme.name" />,
      dataIndex: 'name',
      key: 'name',
      className: 'issue-table-ellipsis',
      filters: [],
      render: (text, record) => {
        const menu = (
          <Menu onClick={item => handleChooseMenu(item.key, record)}>
            <Menu.Item key="editName">
              <span>编辑名称</span>
            </Menu.Item>
            {record.status === 'create'
              && (
                <Menu.Item key="delete">
                  <FormattedMessage id="delete" />
                </Menu.Item>
              )
            }
          </Menu>
        );

        return (
          <TableDropMenu
            menu={menu}
            text={text}
            isHasMenu
            onClickEdit={handleEdit.bind(this, record)}
          />
        );
      },
    },
    {
      title: <FormattedMessage id="stateMachineScheme.project" />,
      dataIndex: 'project',
      key: 'project',
      render: (text, record) => (
        record.projectVOS && record.projectVOS.length
          ? (
            <div className="issue-table-ellipsis">
              <ul className={`${prefixCls}-table-ul`}>
                {record.projectVOS.map(
                  project => (<li>{project ? project.name : ''}</li>),
                )}
              </ul>
            </div>
          )
          : <div>-</div>
      ),
    },
    {
      title: (
        <div className={`${prefixCls}-table-title`}>
          <span className={`${prefixCls}-table-issueType`}>
            <FormattedMessage id="stateMachineScheme.issueType" />
          </span>
          <span className={`${prefixCls}-table-stateMachine`}>
            <FormattedMessage id="stateMachineScheme.stateMachine" />
          </span>
        </div>
      ),
      notDisplay: true,
      dataIndex: 'related',
      key: 'related',
      render: (text, record) => record.configVOS && record.configVOS
        .map(configVO => (
          <Fragment key={configVO.id}>
            <div className={`${prefixCls}-table-related`}>
              <span className={`${prefixCls}-table-issueType`}>
                <TypeTag
                  data={{
                    colour: configVO.issueTypeColour,
                    name: configVO.issueTypeName,
                    icon: configVO.issueTypeIcon,
                  }}
                  showName
                />
              </span>
              <span className={`${prefixCls}-table-arrow`}>
                <Icon
                  type="arrow_forward"
                  style={{ verticalAlign: 'top', marginLeft: 10 }}
                />
              </span>
              <span className={`${prefixCls}-table-stateMachine-content`}>
                {configVO.stateMachineName}
              </span>
            </div>
          </Fragment>
        )),
    },
  ];

  useEffect(() => {
    const { name, id, organizationId } = AppState.currentMenuType;
    stateMachineSchemeStore.loadStateMachineSchemeList(organizationId, {
      current: 1,
      pageSize: 10,
    });
  }, []);

  function render() {
    const { form } = props;
    const {
      pagination,
      getIsLoading,
      getStateMachineSchemeList,
    } = stateMachineSchemeStore;
    const menu = AppState.currentMenuType;
    const {
      type, id, organizationId: orgId, name,
    } = menu;

    const { getFieldDecorator } = form;
    const formContent = (
      <div className="issue-region">
        <Form layout="vertical" className="issue-sidebar-form">
          {showType === 'create' && (
            <FormItem {...formItemLayout}>
              {getFieldDecorator('name', {
                rules: [
                  {
                    required: true,
                    whitespace: true,
                    max: 47,
                    message: intl.formatMessage({ id: 'required' }),
                  }, {
                    validator: checkName,
                  }],
              })(
                <Input
                  placeholder={
                    <FormattedMessage id="stateMachineScheme.createName" />
                  }
                  style={{ width: 520 }}
                  label={<FormattedMessage id="stateMachineScheme.name" />}
                  size="default"
                />,
              )}
            </FormItem>
          )}
          <FormItem {...formItemLayout} className="issue-sidebar-form">
            {getFieldDecorator('description')(
              <TextArea
                placeholder={intl.formatMessage({
                  id: 'stateMachineScheme.createDes',
                })}
                style={{ width: 520 }}
                label={<FormattedMessage id="stateMachineScheme.des" />}
              />,
            )}
          </FormItem>
        </Form>
      </div>
    );

    return (
      <Page>
        <Breadcrumb />
        <Content className="issue-state-content">
          <Table
            dataSource={getStateMachineSchemeList}
            rowClassName={`${prefixCls}-table-col`}
            columns={getColumns()}
            filterBarPlaceholder="过滤表"
            rowKey={record => record.id}
            pagination={pagination}
            onChange={tableChange}
            loading={getIsLoading}
            className="issue-table"
          />
        </Content>
        <Modal
          title={<FormattedMessage id="stateMachineScheme.delete" />}
          visible={stateMachineSchemeStore.getIsSchemeDeleteVisible}
          onOk={confirmDelete.bind(this, deleteId)}
          onCancel={cancelDelete}
          center
        >
          <p className={`${prefixCls}-del-content`}>
            <FormattedMessage id="stateMachineScheme.delete" />
            <span>:</span>
            <span className={`${prefixCls}-del-content-name`}>{deleteItemName}</span>
          </p>
          <p className={`${prefixCls}-del-tip`}>
            <FormattedMessage id="stateMachineScheme.deleteDesAfter" />
          </p>
        </Modal>
        {show && (
          <Sidebar
            title={<FormattedMessage id="stateMachineScheme.create" />}
            onOk={handleSubmit}
            visible={show}
            okText={(
              <FormattedMessage
                id={showType === 'create' ? 'create' : 'save'}
              />
            )}
            cancelText={<FormattedMessage id="cancel" />}
            confirmLoading={submitting}
            onCancel={hideSidebar}
          >
            {formContent}
          </Sidebar>
        )}
      </Page>
    );
  }
  return render();
}

export default Form.create({})(observer(StateMachineSchemeList));
