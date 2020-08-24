/* eslint-disable react/sort-comp */
/* eslint-disable react/prop-types */
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import {
  Page, Header, Content, stores, Permission, Breadcrumb,
} from '@choerodon/boot';
import moment from 'moment';
import {
  Button, Spin, Modal, Tabs, Tooltip, Icon,
} from 'choerodon-ui';
import { withRouter } from 'react-router-dom';
import './Setting.less';
import { commonApi, boardApi } from '@/api';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import ScrumBoardStore from '../../../stores/project/scrumBoard/ScrumBoardStore';
import SettingColumn from './components/setting-column';
import SwimLanePage from './components/SwimLanePage/SwimLanePage';
import WorkCalendarPage from './components/WorkCalendarPage/WorkCalendarPage';
import EditBoardName from './components/EditBoardName/EditBoardName';
import CreateStatus from './components/create-status';
import CreateColumn from './components/create-column';

const { TabPane } = Tabs;
const { confirm } = Modal;
const { AppState } = stores;

const service = [
  'choerodon.code.project.cooperation.iteration-plan.ps.config',
  'choerodon.code.project.cooperation.iteration-plan.ps.status.create',
  'choerodon.code.project.cooperation.iteration-plan.ps.column.create',
  'choerodon.code.project.cooperation.iteration-plan.ps.board.delete',
  'choerodon.code.project.cooperation.iteration-plan.ps.board.update',
  'choerodon.code.project.cooperation.iteration-plan.ps.column',
  'choerodon.code.project.cooperation.iteration-plan.ps.status.update',
  'choerodon.code.project.cooperation.iteration-plan.ps.status.delete',
  'choerodon.code.project.cooperation.iteration-plan.ps.work_calendar.update',
];

@observer
class Setting extends Component {
  constructor(props) {
    super(props);
    this.state = {
      loading: false,
      activeKey: '1',
    };
  }

  componentDidMount() {
    this.refresh();
  }

  refresh=() => {
    this.setState({
      loading: true,
    });
    const boardId = ScrumBoardStore.getSelectedBoard;
    if (!boardId) {
      to(LINK_URL.scrumboard);
    } else {
      ScrumBoardStore.loadStatus();
      boardApi.load(boardId).then((data) => {
        boardApi.loadNoColumnStatus(boardId).then((data2) => {
          const unsetColumn = {
            columnId: '0',
            name: '未对应的状态',
            subStatusDTOS: data2,
          };
          data.columnsData.columns.push(unsetColumn);
          ScrumBoardStore.setBoardData(data.columnsData.columns);
          this.setState({
            loading: false,
          });
        }).catch((error2) => {
        });
      }).catch((error) => {
      });
      commonApi.loadLookupValue('constraint').then((res) => {
        const oldLookup = ScrumBoardStore.getLookupValue;
        oldLookup.constraint = res.lookupValues;
        ScrumBoardStore.setLookupValue(oldLookup);
      }).catch((error) => {
      });
      const year = moment().year();
      ScrumBoardStore.axiosGetWorkSetting(year).then(() => {
        ScrumBoardStore.axiosGetCalendarData(year);
      }).catch(() => {
        ScrumBoardStore.axiosGetCalendarData(year);
      });
      ScrumBoardStore.axiosCanAddStatus();
    }
  }

  handleDeleteBoard() {
    const { name } = ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard);
    confirm({
      title: `删除看板"${name}"`,
      content: '确定要删除该看板吗?',
      okText: '删除',
      cancelText: '取消',
      className: 'scrumBoardMask',
      width: 520,
      onOk() {
        boardApi.delete(ScrumBoardStore.getSelectedBoard).then((res) => {
          to(LINK_URL.scrumboard);
        }).catch((error) => {
        });
      },
      onCancel() {
      },
    });
  }

  handleTabChange=(key) => {
    this.setState({
      activeKey: key,
    }, () => {
      if (key === '4') {
        // setTimeout(() => {
        //   this.input.focus();
        // }, 100);
      }
    });
  }

  handleCreateStatusClick=() => {
    CreateStatus.open({
      onCreate: this.refresh,
    });
  }

  handleCreateColumnClick=() => {
    CreateColumn.open({
      onCreate: this.refresh,
      statusList: ScrumBoardStore.getStatusList,
      sequence: ScrumBoardStore.getBoardData.length - 1,
      boardId: ScrumBoardStore.getSelectedBoard,
    });
  }

  renderWorkCalendarPage = (updateWorkDatePermission) => (
    <WorkCalendarPage selectedDateDisabled={!updateWorkDatePermission} />
  )

  renderEditBoardName = (editBoardNamePermission) => (
    <EditBoardName
      editBoardNameDisabled={!editBoardNamePermission}
      saveRef={(ref) => {
        this.input = ref;
      }}
    />
  )

  render() {
    const { loading, activeKey } = this.state;
    const menu = AppState.currentMenuType;
    const { type, id: projectId, organizationId: orgId } = menu;
    return (
      <Page
        service={service}
      >
        <Header title="配置看板">
          {activeKey === '1' ? (
            <>
              {
              ScrumBoardStore.getCanAddStatus ? (
                <Permission service={['choerodon.code.project.cooperation.iteration-plan.ps.status.create']}>
                  <Button
                    icon="playlist_add"
                    onClick={this.handleCreateStatusClick}
                  >
                    添加状态
                  </Button>
                </Permission>
              ) : (
                <Tooltip
                  placement="bottomLeft"
                  title="当前项目关联了多个状态机，无法创建状态。"
                  getPopupContainer={(triggerNode) => triggerNode.parentNode}
                >
                  <Button
                    funcType="flat"
                    type="primary"
                    disabled
                  >
                    <Icon type="playlist_add" />
                    <span>添加状态</span>
                  </Button>
                </Tooltip>
              )
            }
              <Permission service={['choerodon.code.project.cooperation.iteration-plan.ps.column.create']}>
                <Button
                  icon="playlist_add"
                  onClick={this.handleCreateColumnClick}
                >
                  添加列
                </Button>
              </Permission>
            </>
          ) : null}
          <Permission service={['choerodon.code.project.cooperation.iteration-plan.ps.board.delete']}>
            <Button funcType="flat" onClick={() => this.handleDeleteBoard()} disabled={ScrumBoardStore.getBoardList.size === 1}>
              <Icon type="delete_forever icon" />
              <span>删除看板</span>
            </Button>
          </Permission>
        </Header>
        <Breadcrumb title="配置看板" />
        <Content className="c7n-scrumboard" style={{ height: '100%', paddingTop: 0 }}>
          <Tabs
            style={{
              display: 'flex', flexDirection: 'column', height: '100%', overflow: 'auto',
            }}
            activeKey={activeKey}
            onChange={this.handleTabChange}
          >
            <TabPane tab="列配置" key="1">
              <Spin spinning={loading}>
                <SettingColumn
                  refresh={this.refresh}
                />
              </Spin>
            </TabPane>
            <TabPane tab="泳道" key="2">
              <SwimLanePage />
            </TabPane>
            {ScrumBoardStore.getCalanderCouldUse
              ? (
                <TabPane tab="工作日历" key="3">
                  <Permission service={['choerodon.code.project.cooperation.iteration-plan.ps.work_calendar.update']}>
                    {this.renderWorkCalendarPage}
                  </Permission>
                </TabPane>
              ) : null}
            <TabPane tab="看板名称" key="4">
              <Permission service={['choerodon.code.project.cooperation.iteration-plan.ps.board.update']}>
                {this.renderEditBoardName}
              </Permission>
            </TabPane>
          </Tabs>
        </Content>
      </Page>
    );
  }
}

export default withRouter(Setting);
export { service };
