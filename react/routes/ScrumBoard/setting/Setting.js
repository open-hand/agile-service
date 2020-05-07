import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import {
  Page, Header, Content, stores, axios, Permission, Breadcrumb,
} from '@choerodon/boot';
import moment from 'moment';
import {
  Button, Spin, Modal, Tabs, Tooltip, Icon,
} from 'choerodon-ui';
import { withRouter } from 'react-router-dom';
import './Setting.less';
import ScrumBoardStore from '../../../stores/project/scrumBoard/ScrumBoardStore';
import SettingColumn from './components/setting-column';
import SwimLanePage from './components/SwimLanePage/SwimLanePage';
import WorkcalendarPage from './components/WorkCalendarPage/WorkCalendarPage';
import EditBoardName from './components/EditBoardName/EditBoardName';
import CreateStatus from './components/create-status';
import CreateColumn from './components/create-column';

const { TabPane } = Tabs;
const { confirm } = Modal;
const { AppState } = stores;

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
      const { history } = this.props;
      const urlParams = AppState.currentMenuType;
      history.push(`/agile/scrumboard?type=${urlParams.type}&id=${urlParams.id}&name=${encodeURIComponent(urlParams.name)}&organizationId=${urlParams.organizationId}&orgId=${urlParams.organizationId}`);
    } else {
      ScrumBoardStore.loadStatus();
      ScrumBoardStore.axiosGetBoardDataBySetting(boardId).then((data) => {
        ScrumBoardStore.axiosGetUnsetData(boardId).then((data2) => {
          const unsetColumn = {
            columnId: 0,
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
      ScrumBoardStore.axiosGetLookupValue('constraint').then((res) => {
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
    const { history } = this.props;
    const urlParams = AppState.currentMenuType;
    const { name } = ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard);
    confirm({
      title: `删除看板"${name}"`,
      content: '确定要删除该看板吗?',
      okText: '删除',
      cancelText: '取消',
      className: 'scrumBoardMask',
      width: 520,
      onOk() {
        ScrumBoardStore.axiosDeleteBoard().then((res) => {
          history.push(`/agile/scrumboard?type=${urlParams.type}&id=${urlParams.id}&name=${encodeURIComponent(urlParams.name)}&organizationId=${urlParams.organizationId}&orgId=${urlParams.organizationId}`);
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

  renderWorkcalendarPage = updateWorkDatePermission => (
    <WorkcalendarPage selectedDateDisabled={!updateWorkDatePermission} />
  )

  renderEditBoardName = editBoardNamePermission => (
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
        service={[
          'agile-service.quick-filter.listByProjectId',
          'base-service.project.list',
          'agile-service.board.queryByProjectId',
          'agile-service.board.checkName',
          'agile-service.board.createScrumBoard',
          'agile-service.board.move',
          'agile-service.board.deleteScrumBoard',
          'agile-service.scheme.queryStatusByProjectId',
          'base-service.time-zone-work-calendar-project.queryTimeZoneWorkCalendarDetail',
          'agile-service.scheme.checkCreateStatusForAgile',
          'agile-service.work-calendar-ref.querySprintWorkCalendarRefs',
          'agile-service.issue-status.listUnCorrespondStatus',
          'agile-service.board.updateUserSettingBoard',
          'agile-service.board.updateScrumBoard',
          'agile-service.issue-status.moveStatusToColumn',
          'agile-service.board-column.deleteBoardColumn',
          'agile-service.scheme.checkRemoveStatusForAgile',
          'agile-service.issue-status.deleteStatus',
          'agile-service.board-column.columnSortByProgram',
          'agile-service.board-column.createBoardColumn',
          'agile-service.board-column.updateColumnContraint',
          'agile-service.lookup-value.queryLookupValueByCode',
          'agile-service.status.checkName',
          'agile-service.work-calendar-ref.deleteProjectWorkCalendarRef',
          'agile-service.work-calendar-ref.createSprintWorkCalendarRef',
          'agile-service.board-column.columnSort',
          'agile-service.issue-status.moveStatusToUnCorrespond',
          'agile-service.issue-status.updateStatus',
          'agile-service.issue-status.createStatus',
        ]}
      >
        <Header title="配置看板">
          {activeKey === '1' ? (
            <Fragment>
              {
              ScrumBoardStore.getCanAddStatus ? (
                <Permission type={type} projectId={projectId} organizationId={orgId} service={['agile-service.issue-status.createStatus']}>
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
                  getPopupContainer={triggerNode => triggerNode.parentNode}
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
              <Permission type={type} projectId={projectId} organizationId={orgId} service={['agile-service.board-column.createBoardColumn']}>
                <Button
                  icon="playlist_add"
                  onClick={this.handleCreateColumnClick}
                >
                  添加列
                </Button>
              </Permission>
            </Fragment>
          ) : null}
          <Permission type={type} projectId={projectId} organizationId={orgId} service={['agile-service.board.deleteScrumBoard']}>
            <Button funcType="flat" onClick={this.handleDeleteBoard.bind(this)} disabled={ScrumBoardStore.getBoardList.size === 1}>
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
                  refresh={this.refresh.bind(this)}
                />
              </Spin>
            </TabPane>
            <TabPane tab="泳道" key="2">
              <SwimLanePage />
            </TabPane>
            {ScrumBoardStore.getCalanderCouldUse
              ? (
                <TabPane tab="工作日历" key="3">
                  <Permission service={['agile-service.work-calendar-ref.createSprintWorkCalendarRef', 'agile-service.work-calendar-ref.deleteProjectWorkCalendarRef']}>
                    {this.renderWorkcalendarPage}
                  </Permission>
                </TabPane>
              ) : null
            }
            <TabPane tab="看板名称" key="4">
              <Permission service={['agile-service.board.updateScrumBoard']}>
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
