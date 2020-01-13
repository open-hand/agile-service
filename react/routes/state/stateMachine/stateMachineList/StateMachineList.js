import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import {
  Table, Button, Modal, Form,
  Input, Tooltip, message, Icon, Menu,
} from 'choerodon-ui';
import { injectIntl, FormattedMessage } from 'react-intl';
import {
  Content, Header, TabPage as Page, stores, Breadcrumb, Choerodon,
} from '@choerodon/boot';
import './StateMachineList.less';
import TableDropMenu from '../../../../common/TableDropMenu';

const { AppState } = stores;
const { Sidebar } = Modal;
const FormItem = Form.Item;
const prefixCls = 'issue-state-machine';


@observer
class StateMachineList extends Component {
  constructor(props) {
    const menu = AppState.currentMenuType;
    super(props);
    this.state = {
      page: 1,
      pageSize: 10,
      // id 为状态机id name为编辑时回显的名字
      id: '',
      name: '',
      objectVersionNumber: '',
      organizationId: menu.organizationId,
      // projectId: menu.id,
      // openRemove: false,
      show: false,
      submitting: false,
      statesMachineList: [],
    };
  }

  componentDidMount() {
    this.loadStateMachine();
  }

  handleEditName(record) {
    this.showSideBar('edit', record.id, record.name, record.objectVersionNumber);
  }

  handleChooseMenu(key, record) {
    if (key === 'edit') {
      this.handleEditName(record);
    } else if (key === 'del') {
      this.confirmDelete(record);
    }
  }

  renderMenu = (text, record) => {
    const menu = (
      <Menu onClick={item => this.handleChooseMenu(item.key, record)}>
        <Menu.Item key="edit">
          {/* <Tooltip placement="top" title={<FormattedMessage id="edit" />}> */}
          <span>编辑名称</span>
          {/* </Tooltip> */}
        </Menu.Item>
        {(!(record && record.stateMachineSchemeVOS && record.stateMachineSchemeVOS.length)
          && !record.default) && (
            <Menu.Item key="del">
              {/* <Tooltip placement="top" title={<FormattedMessage id="delete" />}> */}
              <span>删除</span>
              {/* </Tooltip> */}
            </Menu.Item>
        )
        }

      </Menu>
    );
    return (
      <TableDropMenu
        menu={menu}
        onClickEdit={this.handleEdit.bind(this, record.id, record.status)}
        isHasMenu
        text={text}
      />
    );
  };

  getColumn = () => ([{
    title: <FormattedMessage id="stateMachine.name" />,
    dataIndex: 'name',
    key: 'name',
    filters: [],
    render: (text, record) => this.renderMenu(text, record),
  }, {
    title: <FormattedMessage id="stateMachine.related" />,
    dataIndex: 'stateMachineSchemeVOS',
    key: 'stateMachineSchemeVOS',
    className: 'issue-table-ellipsis',
    render: (text, record) => {
      const map = [];
      if (text && text.length) {
        map.push(text.map(data => (
          <li key={data.id}>
            <a onClick={() => this.handleSchemeClick(data.id)} role="none">{data.name}</a>
          </li>
        )));
      } else {
        return <div>-</div>;
      }
      return <ul className={`${prefixCls}-related`}>{map}</ul>;
    },
  }]);

  showSideBar = (state, id = '', name = '', objectVersionNumber = '') => {
    this.setState({
      show: true,
      type: state,
      id,
      name,
      objectVersionNumber,
    });
  };

  hideSidebar = () => {
    this.setState({
      show: false,
      type: '',
      id: '',
      name: '',
      objectVersionNumber: '',
    });
  };

  confirmDelete = (record) => {
    this.setState({
      deleteVisible: true,
      deleteId: record.id,
      deleteName: record.name,
    });
  };

  handleSchemeClick = (schemeId) => {
    const { history } = this.props;
    const { name, id, organizationId } = AppState.currentMenuType;
    history.push(`/agile/states/scheme/edit/${schemeId}?type=organization&id=${id}&name=${encodeURIComponent(name)}&organizationId=${organizationId}&orgId=${organizationId}&fromMachine=true`);
  };

  loadStateMachine = (page = 1, size = 10, sort = { field: 'id', order: 'desc' }, param = {}) => {
    const { StateMachineStore } = this.props;
    const { organizationId } = this.state;
    StateMachineStore.loadStateMachineList(organizationId, sort, { page, size, ...param })
      .then((data) => {
        this.setState({
          statesMachineList: data.list,
          total: data.total,
        });
      });
  };

  tableChange = (pagination, filters, sorter, param) => {
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
    this.setState({
      page: pagination.current,
      pageSize: pagination.pageSize,
      sorter: sorter.column ? sorter : undefined,
      tableParam: postData,
    });
    this.loadStateMachine(pagination.current, pagination.pageSize, sorter.column ? sorter : undefined, postData);
  };

  handleSubmit = () => {
    const { StateMachineStore } = this.props;
    const {
      id,
      type,
      editState,
      page,
      pageSize,
      sorter,
      tableParam,
      organizationId,
    } = this.state;

    this.props.form.validateFieldsAndScroll((err, data) => {
      if (!err) {
        const postData = data;
        postData.organizationId = organizationId;
        this.setState({
          submitting: true,
        });
        if (type === 'create') {
          StateMachineStore.createStateMachine(organizationId, postData)
            .then((res) => {
              if (res) {
                this.loadStateMachine(page, pageSize, sorter, tableParam);
                this.setState({ type: false, show: false });
              }
              this.setState({
                submitting: false,
              });
            }).catch((error) => {
              Choerodon.prompt(error.response.data.message);
              this.setState({
                submitting: false,
              });
            });
        } else {
          const { objectVersionNumber } = this.state;
          postData.objectVersionNumber = objectVersionNumber;
          StateMachineStore.updateStateMachine(organizationId, id, postData)
            .then((res) => {
              if (res) {
                this.loadStateMachine(page, pageSize, sorter, tableParam);
                this.setState({ type: false, show: false });
              }
              this.setState({
                submitting: false,
              });
            }).catch((error) => {
              Choerodon.prompt(error.response.data.message);
              this.setState({
                submitting: false,
              });
            });
        }
      }
    });
  };

  closeRemove = () => {
    this.setState({
      deleteVisible: false,
      deleteId: '',
    });
  };

  handleDelete = () => {
    const { StateMachineStore, intl } = this.props;
    const {
      organizationId, deleteId, page, pageSize, sorter, tableParam,
    } = this.state;
    StateMachineStore.deleteStateMachine(organizationId, deleteId)
      .then((data) => {
        if (data) {
          message.success(intl.formatMessage({ id: 'deleteSuccess' }));
        } else {
          message.error(intl.formatMessage({ id: 'deleteFailed' }));
        }
        this.closeRemove();
        this.loadStateMachine(page, pageSize, sorter, tableParam);
      }).catch((error) => {
        message.error(intl.formatMessage({ id: 'deleteFailed' }));
        this.closeRemove();
      });
  };

  refresh = () => {
    this.loadStateMachine();
  };

  closeRemove = () => {
    this.setState({
      deleteVisible: false, deleteId: false,
    });
  };

  handleEdit = (stateMachineId, status) => {
    const { StateMachineStore, intl, history } = this.props;
    const { name, id, organizationId } = AppState.currentMenuType;
    history.push(`/agile/states/state-machine/edit/${stateMachineId}/${status === 'state_machine_create' ? status : 'state_machine_active'}?type=organization&id=${id}&name=${encodeURIComponent(name)}&organizationId=${organizationId}&orgId=${organizationId}`);
  };

  checkName = async (rule, value, callback) => {
    const { StateMachineStore, intl } = this.props;
    const orgId = AppState.currentMenuType.organizationId;
    const res = await StateMachineStore.checkName(orgId, value);
    if (res) {
      callback(intl.formatMessage({ id: 'priority.create.name.error' }));
    } else {
      callback();
    }
  };

  render() {
    const { StateMachineStore, intl } = this.props;
    const {
      statesMachineList,
      page,
      pageSize,
      total,
      deleteName,
      name: StateName,
    } = this.state;
    const { getFieldDecorator } = this.props.form;
    const formContent = (
      <div className="issue-region">
        <Form layout="vertical" className="issue-sidebar-form">
          <FormItem>
            {getFieldDecorator('name',
              {
                initialValue: StateName,
                rules: [{
                  required: true,
                  whitespace: true,
                  max: 47,
                  message: intl.formatMessage({ id: 'required' }),
                }, {
                  validator: this.checkName,
                }],
              })(
                <Input 
                  autoFocus
                  label={<FormattedMessage id="stateMachine.name" />}
                />,
            )}
          </FormItem>
        </Form>
      </div>
    );
    const pageInfo = {
      defaultCurrent: page,
      defaultPageSize: pageSize,
      total,
    };
    return (
      <Page
        service={[
          'agile-service.state-machine.pagingQuery',
          'agile-service.state-machine.create',
          'agile-service.state-machine.checkName',
          'agile-service.state-machine.delete',
        ]}
      >
        <Header>
          {statesMachineList && statesMachineList.length === 0
            ? (
              <Tooltip placement="bottom" title="请创建项目后再创建状态机">
                <Button
                  disabled
                >
                  <Icon type="playlist_add" />
                  <FormattedMessage id="stateMachine.create" />
                </Button>
              </Tooltip>
            )
            : (
              <Button
                onClick={() => this.showSideBar('create')}
              >
                <Icon type="playlist_add" />
                <FormattedMessage id="stateMachine.create" />
              </Button>
            )
          }
        </Header>
        <Breadcrumb />
        <Content className="issue-state-content">
          <Table
            dataSource={statesMachineList}
            columns={this.getColumn()}
            filterBarPlaceholder="过滤表"
            rowKey={record => record.id}
            loading={StateMachineStore.getIsLoading}
            pagination={pageInfo}
            onChange={this.tableChange}
            className="issue-table"
          />
        </Content>
        {this.state.show && (
          <Sidebar
            title={<FormattedMessage id={this.state.type === 'create' ? 'stateMachine.create' : 'stateMachine.edit'} />}
            visible={this.state.show}
            onOk={this.handleSubmit}
            okText={<FormattedMessage id={this.state.type === 'create' ? 'create' : 'save'} />}
            cancelText={<FormattedMessage id="cancel" />}
            confirmLoading={this.state.submitting}
            onCancel={this.hideSidebar}
            width={380}
          >
            {formContent}
          </Sidebar>
        )}

        <Modal
          title={<FormattedMessage id="stateMachine.delete" />}
          visible={this.state.deleteVisible}
          onOk={this.handleDelete}
          onCancel={this.closeRemove}
        >
          <p className={`${prefixCls}-del-content`}>
            <FormattedMessage id="stateMachine.delete" />
            <span>:</span>
            <span className={`${prefixCls}-del-content-name`}>{deleteName}</span>
          </p>
          <p className={`${prefixCls}-del-tip`}>
            <FormattedMessage id="state.delete.tip" />
          </p>
        </Modal>
      </Page>
    );
  }
}

export default Form.create({})(withRouter(injectIntl(StateMachineList)));
