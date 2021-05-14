/* eslint-disable react/jsx-no-bind */
import React, {
  useState, useEffect, useContext,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Table, Button, Modal, Form, Select, Input, Tooltip, Menu,
} from 'choerodon-ui';
import { Button as ButtonPro } from 'choerodon-ui/pro';
import { FormattedMessage } from 'react-intl';
import {
  Content, Header, TabPage as Page, Breadcrumb, Choerodon, useTheme,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import { getStageMap, getStageList } from '@/utils/stateMachine';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { statusApi } from '@/api';
import Store from './stores';
import './StateList.less';
import TableDropMenu from '../../../common/TableDropMenu';

const backlogStates = ['backlog_pending_approval', 'backlog_rejected', 'backlog_create', 'backlog_planning', 'backlog_processing', 'backlog_developed', 'backlog_publish'];
const { Sidebar, info } = Modal;
const FormItem = Form.Item;
const { TextArea } = Input;
const { Option } = Select;
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
const prefixCls = 'issue-state';

const stageMap = getStageMap();
const stageList = getStageList();

function StateList(props) {
  const [theme] = useTheme();
  const context = useContext(Store);
  const { AppState, stateStore, intl: { formatMessage } } = context;
  const {
    name, type, id, organizationId: orgId,
  } = AppState.currentMenuType;
  const [submitting, setSubmitting] = useState(false);
  const [show, setShow] = useState(false);
  const [showType, setShowType] = useState('');
  //  这三个放到一起管理
  const [deleteVisible, setDeleteVisible] = useState(false);
  const [deleteId, setDeleteId] = useState('');
  const [deleteName, setDeleteName] = useState('');

  const [editState, setEditState] = useState(false);

  const [statesList, setStatesList] = useState({
    list: [],
    total: 0,
  });
  const [initialTotal, setInitialTotal] = useState(0);

  const [pagination, setPagination] = useState({
    page: 1,
    pageSize: 10,
    total: 0,
  });
  const [tableParam, setTableParam] = useState({
    sorter: undefined,
    param: {},
    page: 1,
    pageSize: 10,
  });
  let modelRef = false;

  const linkToStateMachine = (machineId, status) => {
    modelRef.destroy();
    const { history } = context;
    const { organizationId } = AppState.currentMenuType;
    history.push(`/agile/states/state-machine/edit/${machineId}/${status || 'state_machine_active'}?type=organization&id=${id}&name=${encodeURIComponent(name)}&organizationId=${organizationId}&orgId=${organizationId}`);
  };

  const showStateMachines = (data) => {
    modelRef = info({
      title: `${data.name}关联的状态机`,
      content: (
        <ul className="issue-state-ul">
          {
            data.stateMachineInfoList.map((stateMachine) => (
              <li key={stateMachine.stateMachineId}>
                <a
                  role="none"
                  onClick={() => linkToStateMachine(
                    stateMachine.stateMachineId, stateMachine.stateMachineStatus,
                  )}
                >
                  {stateMachine.stateMachineName}
                </a>
              </li>
            ))
          }
        </ul>
      ),
      onOk() { },
      okText: formatMessage({ id: 'confirm' }),
    });
  };

  const showSideBar = (state, newId = '') => {
    if (state === 'edit') {
      statusApi.load(newId).then((data) => {
        if (data && data.failed) {
          Choerodon.prompt(data.message);
        } else {
          setEditState(data);
        }
      });
    }
    setShow(true);
    setShowType(state);
  };

  const confirmDelete = (record) => {
    setDeleteId(record.id);
    setDeleteName(record.name);
    setDeleteVisible(true);
  };

  const handleCancel = () => {
    setDeleteId('');
    setDeleteVisible(false);
  };

  const loadState = ({
    page = 1, size = 10, sort = { field: 'id', order: 'desc' }, param = {}, isSetInitialTotal = false,
  }) => {
    stateStore.loadStateList(orgId, page, size, sort, param, isSetInitialTotal).then((data) => {
      setStatesList({
        list: data.list,
        total: data.total,
      });
      setPagination({
        page,
        pageSize: size,
        total: data.total,
      });
      if (isSetInitialTotal) {
        setInitialTotal(data.total);
      }
    });
  };

  const handleDelete = () => {
    statusApi.delete(deleteId).then((data) => {
      if (data && data.failed) {
        Choerodon.prompt(data.message);
      } else {
        loadState({
          page: pagination.page, size: pagination.pageSize, sort: tableParam.sorter, param: tableParam.param,
        });
        setDeleteId('');
        setDeleteVisible(false);
      }
    });
  };

  const hideSidebar = () => {
    setShow(false);
    setShowType('');
    setEditState(false);
  };

  const handleSubmit = () => {
    const { form } = props;
    // const {
    //   type, page, pageSize, sorter, tableParam,
    // } = this.state;
    form.validateFieldsAndScroll((err, data) => {
      if (!err) {
        const postData = data;
        setSubmitting(true);
        if (showType === 'create') {
          statusApi.create(postData)
            .then((res) => {
              if (res && res.failed) {
                // eslint-disable-next-line no-console
                console.log(res.message);
              } else {
                loadState({
                  page: pagination.page, size: pagination.pageSize, sort: tableParam.sorter, param: tableParam.param,
                });
                setShowType(false);
                setShow(false);
                setEditState(false);
              }

              setSubmitting(false);
            }).catch((error) => {
              setSubmitting(false);
            });
        } else {
          statusApi.update(editState.id, Object.assign(editState, postData))
            .then((res) => {
              if (res && res.failed) {
                Choerodon.prompt(res.message);
              } else {
                loadState({
                  page: pagination.page, size: pagination.pageSize, sort: tableParam.sorter, param: tableParam.param,
                });
                setShowType(false);
                setShow(false);
                // 此处值 应传入布尔类型是吗
                setEditState({});
              }
              setSubmitting(false);
            });
        }
      }
    });
  };

  const refreshData = () => {
    loadState({
      page: pagination.page, size: pagination.pageSize, sort: tableParam.sorter, param: tableParam.param,
    });
  };

  const tableChange = (newPagination, filters, sorter, param) => {
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
    if (filters && filters.type && filters.type.length) {
      searchParam = {
        ...searchParam,
        type: filters.type[0],
      };
    }
    if (param && param.length) {
      searchParam = {
        ...searchParam,
        param: param.toString(),
      };
    }
    setTableParam({
      page: newPagination.current,
      pageSize: newPagination.pageSize,
      sorter: sorter.column ? sorter : undefined,
      param: searchParam,
    });
    loadState({
      page: newPagination.current,
      size: newPagination.pageSize,
      sort: sorter.column ? sorter : undefined,
      param: searchParam,
    });
  };

  const checkName = async (rule, value, callback) => {
    if (!value || !value.trim()) {
      callback();
      return;
    }
    // const { type, editState } = this.state;
    if (showType === 'create' || value !== (editState && editState.name)) {
      setSubmitting(true);
      const res = await statusApi.checkName(value);
      setSubmitting(false);
      if (res && res.statusExist) {
        callback(formatMessage({ id: 'priority.create.name.error' }));
      } else {
        callback();
      }
    } else {
      callback();
    }
  };
  const renderMenu = (text, record) => {

  };
  const getColumn = () => ([{
    title: <FormattedMessage id="state.name" />,
    dataIndex: 'name',
    key: 'name',
    filters: [],
    render: (text, record) => {
      const menu = (
        <Menu onClick={confirmDelete.bind(this, record)}>
          <Menu.Item key="del">
            <Tooltip placement="top" title={<FormattedMessage id="delete" />}>
              <span>
                删除
              </span>
            </Tooltip>
          </Menu.Item>
        </Menu>
      );
      return (
        <TableDropMenu
          menu={menu}
          isHasMenu={!(record.code || (record.stateMachineInfoList && record.stateMachineInfoList.length))}
          onClickEdit={showSideBar.bind(this, 'edit', record.id)}
          text={text}
        />
      );
    },
  },
  {
    title: <FormattedMessage id="state.des" />,
    dataIndex: 'description',
    key: 'description',
    filters: [],
    className: 'issue-table-ellipsis',
  },
  {
    title: <FormattedMessage id="state.stage" />,
    dataIndex: 'type',
    key: 'type',
    filters: stageList.filter((s) => s.code !== 'none').map((s) => ({ text: s.name, value: s.code })),
    render: (record) => (
      <div>
        <div className="issue-state-block" style={{ backgroundColor: stageMap[record]?.colour }} />
        <span style={{ verticalAlign: 'middle' }}>{stageMap[record]?.name}</span>
      </div>
    ),
  }]);

  useEffect(() => {
    loadState({
      page: undefined,
      size: undefined,
      sort: undefined,
      param: undefined,
      isSetInitialTotal: true,
    });
  }, []);

  function render() {
    const { form } = props;
    // const {
    //   statesList = [], initialTotal, total, type,
    // } = this.state;
    const disabledEditName = editState && backlogStates.includes(editState.code);
    const { getFieldDecorator } = form;
    const formContent = (
      <div className="issue-region">
        <Form layout="vertical" className="issue-sidebar-form">
          <FormItem
            {...formItemLayout}
          >
            {getFieldDecorator('name', {
              rules: [{
                required: true,
                whitespace: true,
                max: 47,
                message: formatMessage({ id: 'state.name.required' }),
              }, {
                validator: checkName,
              }],
              initialValue: editState ? editState.name : '',
            })(
              <Input
                autoFocus
                label={<FormattedMessage id="state.name" />}
                size="default"
                disabled={disabledEditName}
                maxLength={15}
              />,
            )}
            {disabledEditName && (
            <span style={{
              color: 'rgba(0,0,0,0.65)',
              marginLeft: 2,
            }}
            >
              状态被需求池使用，不可更改名称
            </span>
            )}
          </FormItem>
          <FormItem
            {...formItemLayout}
            className="issue-sidebar-form"
          >
            {getFieldDecorator('description', {
              initialValue: editState ? editState.description : '',
            })(
              <TextArea
                label={<FormattedMessage id="state.des" />}
                maxLength={45}
              />,
            )}
          </FormItem>
          <FormItem
            {...formItemLayout}
          >
            {getFieldDecorator('type', {
              rules: [{
                required: true,
                whitespace: true,
                message: formatMessage({ id: 'required' }),
              }],
              initialValue: editState ? editState.type : 'todo',
            })(
              <Select
                label={<FormattedMessage id="state.stage" />}
                dropdownMatchSelectWidth
                size="default"
              >
                {stageList.map((stage) => (
                  <Option
                    value={stage.code}
                    key={stage.code}
                  >
                    <div style={{ display: 'inline-block' }}>
                      <div className="issue-state-block" style={{ backgroundColor: stage.colour }} />
                      <span style={{ verticalAlign: 'middle', width: '100%' }}>{stage.name}</span>
                    </div>
                  </Option>
                ))}
              </Select>,
            )}
          </FormItem>
        </Form>
      </div>
    );

    const pageInfo = {
      current: pagination.page,
      pageSize: pagination.pageSize,
      total: pagination.total,
    };

    return (
      <Page>
        <Header title={<FormattedMessage id="state.title" />}>
          <HeaderButtons items={[
            {
              name: formatMessage({ id: 'state.create' }),
              icon: 'playlist_add',
              display: true,
              disabled: !initialTotal,
              handler: () => showSideBar('create'),
              tooltipsConfig: {
                hidden: initialTotal,
                title: '请创建项目后再创建状态机',
              },
            },
          ]}
          />
        </Header>
        <Breadcrumb />
        <Content className="issue-state-content" style={theme === 'theme4' ? undefined : { paddingTop: 0 }}>
          <Table
            dataSource={statesList.list}
            columns={getColumn()}
            filterBarPlaceholder="过滤表"
            rowKey={(record) => record.id}
            loading={stateStore.getIsLoading}
            pagination={pageInfo}
            onChange={tableChange}
            className="issue-table"
          />
        </Content>
        {show && (
          <Sidebar
            maskClosable
            title={<FormattedMessage id={showType === 'create' ? 'state.create' : 'state.edit'} />}
            visible={show}
            okText={<FormattedMessage id={showType === 'create' ? 'create' : 'save'} />}
            cancelText={<FormattedMessage id="cancel" />}
            confirmLoading={submitting}
            footer={[
              <ButtonPro key="submit" color="primary" funcType="raised" loading={submitting} onClick={handleSubmit}>
                <FormattedMessage id={showType === 'create' ? 'create' : 'save'} />
              </ButtonPro>,
              <ButtonPro key="back" funcType="raised" onClick={hideSidebar}><FormattedMessage id="cancel" /></ButtonPro>,
            ]}
            width={MODAL_WIDTH.small}
          >
            {formContent}
          </Sidebar>
        )}
        <Modal
          title={<FormattedMessage id="state.delete" />}
          visible={deleteVisible}
          onOk={handleDelete}
          onCancel={handleCancel}
        >
          <p className={`${prefixCls}-del-content`}>
            <FormattedMessage id="state.delete" />
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
  return render();
}
// withRouter 原先在这包裹
export default Form.create({})(observer(StateList));
