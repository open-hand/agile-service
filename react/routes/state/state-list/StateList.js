/* eslint-disable react/jsx-no-bind */
import React, {
  useState, useEffect, useContext, useCallback,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Table, Form, Icon,
} from 'choerodon-ui';
import { Dropdown, Menu } from 'choerodon-ui/pro';
import { FormattedMessage } from 'react-intl';
import {
  Content, Header, TabPage as Page, Breadcrumb, useTheme,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import { getStageMap, getStageList } from '@/utils/stateMachine';
import Store from './stores';
import openStateModal from './StateModal';
import openDeleteModal from './components/DeleteModal';
import { Loading } from '@/components';
import Style from './index.less';

const backlogStates = ['backlog_pending_approval', 'backlog_rejected', 'backlog_create', 'backlog_planning', 'backlog_processing', 'backlog_developed', 'backlog_publish'];

const stageMap = getStageMap();
const stageList = getStageList();

function StateList(props) {
  const [theme] = useTheme();
  const context = useContext(Store);
  const { AppState, stateStore, intl: { formatMessage } } = context;
  const {
    organizationId: orgId,
  } = AppState.currentMenuType;
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

  const loadState = useCallback(({
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
  }, [orgId, stateStore]);

  const handleOnOk = useCallback(() => {
    loadState({
      page: pagination.page, size: pagination.pageSize, sort: tableParam.sorter, param: tableParam.param,
    });
  }, [loadState, pagination.page, pagination.pageSize, tableParam.param, tableParam.sorter]);

  const confirmDelete = (record) => {
    openDeleteModal({ id: record.id, name: record.name, onOk: handleOnOk });
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

  const getColumn = () => ([{
    title: <FormattedMessage id="state.name" />,
    dataIndex: 'name',
    key: 'name',
    filters: [],
    render: (text, record) => (
      <span>{text}</span>
    ),
  },
  {
    dataIndex: 'action',
    key: 'action',
    render: (text, record) => {
      const handleMenuClick = (e) => {
        switch (e.key) {
          case 'edit': {
            openStateModal({
              onOk: handleOnOk, statusId: record.id, name: record.name, disabledEditName: backlogStates.includes(record.code),
            });
            break;
          }
          case 'delete': {
            confirmDelete(record);
            break;
          }
          default: {
            break;
          }
        }
      };
      const menu = (
        // eslint-disable-next-line react/jsx-no-bind
        <Menu onClick={handleMenuClick.bind(this)}>
          <Menu.Item key="edit">编辑</Menu.Item>
          {
            !(record.code || (record.stateMachineInfoList && record.stateMachineInfoList.length)) && (
              <Menu.Item key="delete">删除</Menu.Item>
            )
          }
        </Menu>
      );
      return (
        <Dropdown
          overlay={menu}
          trigger={['click']}
        >
          <Icon
            type="more_vert"
            style={{
              fontSize: 18,
              cursor: 'pointer',
              color: 'var(--primary-color)',
            }}
          />
        </Dropdown>
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
  }, [loadState]);

  function render() {
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
              handler: () => { openStateModal({ onOk: handleOnOk }); },
              tooltipsConfig: {
                title: initialTotal ? undefined : '请创建项目后再创建状态机',
              },
            },
          ]}
          />
        </Header>
        <Breadcrumb />
        <Content className="issue-state-content" style={theme === 'theme4' ? undefined : { paddingTop: 0 }}>
          <Loading loading={stateStore.getIsLoading} className={Style.tableWrap}>
            <Table
              dataSource={statesList.list}
              columns={getColumn()}
              filterBarPlaceholder="过滤表"
              rowKey={(record) => record.id}
              pagination={pageInfo}
              onChange={tableChange}
              className="issue-table"
            />
          </Loading>
        </Content>
      </Page>
    );
  }
  return render();
}
// withRouter 原先在这包裹
export default Form.create({})(observer(StateList));
