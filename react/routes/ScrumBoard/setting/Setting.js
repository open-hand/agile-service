/* eslint-disable react/sort-comp */
/* eslint-disable react/prop-types */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Page, Header, Content, Permission, Breadcrumb,
} from '@choerodon/boot';
import moment from 'moment';
import {
  Spin, Modal, Tabs,
} from 'choerodon-ui/pro';
import { withRouter } from 'react-router-dom';
import { HeaderButtons } from '@choerodon/master';
import './Setting.less';
import { commonApi, boardApi } from '@/api';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import SelectBoard from '../ScrumBoardComponent/select-board';
import ScrumBoardStore from '../../../stores/project/scrumBoard/ScrumBoardStore';
import SettingColumn from './components/setting-column';
import SwimLanePage from './components/SwimLanePage/SwimLanePage';
import WorkCalendarPage from './components/WorkCalendarPage/WorkCalendarPage';
import EditBoardName from './components/EditBoardName/EditBoardName';
import CreateColumn from './components/create-column';

const { TabPane } = Tabs;
const { confirm } = Modal;
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

  refresh = () => {
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
    Modal.open({
      title: `删除看板"${name}"`,
      children: '确定要删除该看板吗?',
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

  handleTabChange = (key) => {
    this.setState({
      activeKey: key,
    });
  }

  handleCreateStatusClick = () => {
    to(LINK_URL.stateMachine);
  }

  handleCreateColumnClick = () => {
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
    />
  )

  render() {
    const { loading, activeKey } = this.state;
    return (
      <Page>
        <Header title="配置看板">
          <HeaderButtons items={[{
            name: '添加状态',
            icon: 'playlist_add',
            handler: this.handleCreateStatusClick,
            display: activeKey === '1',
            permissions: ['choerodon.code.project.cooperation.iteration-plan.ps.status.create'],
          }, {
            name: '添加列',
            icon: 'playlist_add',
            handler: this.handleCreateColumnClick,
            display: activeKey === '1',
            permissions: ['choerodon.code.project.cooperation.iteration-plan.ps.column.create'],
          }, {
            name: '删除看板',
            icon: 'delete_sweep-o',
            handler: this.handleDeleteBoard,
            display: true,
            disabled: ScrumBoardStore.getBoardList.size === 1,
            permissions: ['choerodon.code.project.cooperation.iteration-plan.ps.board.delete'],
          }, {
            display: true,
            element: <SelectBoard
              createButton={false}
              onChange={(value) => {
                const selectedBoard = ScrumBoardStore.getBoardList.get(value);
                ScrumBoardStore.setSelectedBoard(value);
                ScrumBoardStore.setSwimLaneCode(selectedBoard.userDefaultBoard);
                this.refresh(selectedBoard);
              }}
            />,
          }]}
          />
        </Header>
        <Breadcrumb title="配置看板" />
        <Content className="c7n-scrumboard c7n-pro-form-float" style={{ height: '100%', paddingTop: 0 }}>
          <Tabs
            style={{
              display: 'flex', flexDirection: 'column', height: '100%', overflow: 'auto',
            }}
            className={activeKey === '1' ? 'c7n-scrumboard-columnsetting-tabs' : ''}
            activeKey={activeKey}
            onChange={this.handleTabChange}
          >
            <TabPane className="c7n-scrumboard-columnsetting-panel" tab="列配置" key="1">
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
