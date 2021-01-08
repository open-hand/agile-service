import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import {
  Page, Header, Content, stores, Breadcrumb, Choerodon, Permission,
} from '@choerodon/boot';
import {
  Button, Select, Spin, Icon, Modal, Form, Tooltip, Radio,
} from 'choerodon-ui';
import { toJS } from 'mobx';
import { Modal as ModalPro } from 'choerodon-ui/pro';
import CloseSprint from '@/components/close-sprint';
import {
  sprintApi, issueApi, epicApi, issueTypeApi, statusApi, boardApi,
} from '@/api';
import LINK_URL from '@/constants/LINK_URL';
import to from '@/utils/to';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import ScrumBoardDataController from './ScrumBoardDataController';
import ScrumBoardStore from '../../../stores/project/scrumBoard/ScrumBoardStore';
import StatusColumn from '../ScrumBoardComponent/StatusColumn/StatusColumn';
import './ScrumBoardHome.less';
import IssueDetail from '../ScrumBoardComponent/IssueDetail/IssueDetail';
import QuickSearch, { QuickSearchEvent } from '../../../components/QuickSearch';
import NoneSprint from '../ScrumBoardComponent/NoneSprint/NoneSprint';
import '../ScrumBoardComponent/RenderSwimLaneContext/RenderSwimLaneContext.less';
import SwimLane from '../ScrumBoardComponent/RenderSwimLaneContext/SwimLane';
import CSSBlackMagic from '../../../components/CSSBlackMagic/CSSBlackMagic';
import HeaderLine from '../../../components/HeaderLine';
import ScrumBoardFullScreen from '../ScrumBoardComponent/ScrumBoardFullScreen';
import CreateBoard from '../ScrumBoardComponent/CreateBoard';
import { service } from '../setting/Setting';
import SelectPriority from './SelectPriority';
import CreateIssue from '../ScrumBoardComponent/create-issue';
import ExpandAllButton from '../ScrumBoardComponent/expand-all-button';

const { Option } = Select;
const { AppState } = stores;

const RadioGroup = Radio.Group;
const RadioButton = Radio.Button;

const style = (swimLaneId) => `
  .${swimLaneId}.c7n-swimlaneContext-itemBodyColumn {
    background-color: rgba(140, 158, 255, 0.12) !important;
  }
  .${swimLaneId}.c7n-swimlaneContext-itemBodyColumn > .c7n-swimlaneContext-itemBodyStatus >  .c7n-swimlaneContext-itemBodyStatus-container {
    border-width: 2px;
    border-style: dashed;
    border-color: #26348b;
  }
  .${swimLaneId}.c7n-swimlaneContext-itemBodyColumn > .c7n-swimlaneContext-itemBodyStatus > .c7n-swimlaneContext-itemBodyStatus-container > .c7n-swimlaneContext-itemBodyStatus-container-statusName {
      visibility: visible !important;
  } 
`;

@Form.create()
@CSSBlackMagic
@inject('AppState', 'HeaderStore')
@observer
class ScrumBoardHome extends Component {
  constructor(props) {
    super(props);
    this.dataConverter = new ScrumBoardDataController();
    this.ref = null;
    this.state = {
      updateParentStatus: null,
    };
  }

  componentDidMount() {
    ScrumBoardStore.setSelectedBoardId('');
    const scrumboardInitValue = localPageCacheStore.getItem('scrumboard');
    if (scrumboardInitValue) {
      const {
        onlyMeChecked, onlyStoryChecked, moreChecked, starBeacon, personalFilters = [], assigneeFilter, sprintFilter, priorityIds,
      } = scrumboardInitValue;

      ScrumBoardStore.addQuickSearchFilter(
        onlyMeChecked,
        onlyStoryChecked,
        starBeacon,
        moreChecked,
        personalFilters,
      );
      assigneeFilter && ScrumBoardStore.addAssigneeFilter(assigneeFilter.map((item) => item.key));
      if (sprintFilter) {
        ScrumBoardStore.addSprintFilter(sprintFilter);
      }
      priorityIds && ScrumBoardStore.setPriority(priorityIds);
    }
    this.getBoard();
    // eslint-disable-next-line react/destructuring-assignment
    const { state } = this.props.location;
    if (state && state.issueId) {
      ScrumBoardStore.setClickedIssue(state.issueId);
    }
  }

  componentWillUnmount() {
    this.dataConverter = null;
    ScrumBoardStore.resetDataBeforeUnmount();
  }

  getBoard = async () => {
    const { location } = this.props;
    const url = this.paramConverter(location.search);
    const boardListData = await boardApi.loadAll();
    const statusLinkages = await boardApi.getStatusLinkages();
    ScrumBoardStore.initBoardList(boardListData);
    ScrumBoardStore.setStatusLinkages(statusLinkages);
    const defaultBoard = boardListData.find((item) => item.userDefault) || boardListData[0];
    if (defaultBoard.boardId) {
      ScrumBoardStore.setSelectedBoardId(defaultBoard.boardId);
      this.refresh(defaultBoard, url, boardListData);
    }
  }

  paramConverter = (url) => {
    const reg = /[^?&]([^=&#]+)=([^&#]*)/g;
    const retObj = {};
    url.match(reg).forEach((item) => {
      const [tempKey, paramValue] = item.split('=');
      const paramKey = tempKey[0] !== '&' ? tempKey : tempKey.substring(1);
      Object.assign(retObj, {
        [paramKey]: paramValue,
      });
    });
    return retObj;
  };

  onQuickSearchChange = (
    onlyMeChecked = false,
    onlyStoryChecked = false,
    starBeacon = false,
    moreChecked,
    personalFilters,
  ) => {
    ScrumBoardStore.addQuickSearchFilter(
      onlyMeChecked,
      onlyStoryChecked,
      starBeacon,
      moreChecked,
      personalFilters,
    );
    localPageCacheStore.mergeSetItem('scrumboard', {
      onlyMeChecked,
      onlyStoryChecked,
      starBeacon,
      moreChecked,
      personalFilters,
    });
    this.refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
  };

  onAssigneeChange = (value) => {
    ScrumBoardStore.addAssigneeFilter(value);
    this.refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
  };

  onSprintChange = (value) => {
    ScrumBoardStore.addSprintFilter(value);
    localPageCacheStore.mergeSetItem('scrumboard', {
      sprintFilter: value,
    });
    this.refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
  }

  handleClearFilter = () => {
    ScrumBoardStore.clearFilter();
    localPageCacheStore.remove('scrumboard');
    QuickSearchEvent.emit('clearQuickSearchSelect');
    this.refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
  }

  /**
   *完成冲刺
   *
   * @memberof ScrumBoardHome
   */
  handleFinishSprint = async () => {
    const sprintId = ScrumBoardStore.getSprintId;
    const completeMessage = await sprintApi.loadSprintAndCountIssue(sprintId);
    CloseSprint({
      completeMessage,
      sprintId,
      afterClose: async () => {
        const axiosGetSprintNotClosed = sprintApi.loadSprints(['sprint_planning', 'started']);
        await axiosGetSprintNotClosed.then((res) => {
          ScrumBoardStore.setSprintNotClosedArray(res);
          ScrumBoardStore.setSelectSprint(undefined);
          ScrumBoardStore.resetCurrentSprintExist();
          this.onSprintChange(undefined);
        });
        this.refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
      },
    });
  };

  changeState = (name, value) => {
    if (name === 'judgeUpdateParent') {
      statusApi.loadTransformStatusByIssue(value.statusId, value.id, value.typeId).then((types) => {
        this.matchStatus(types);
        this.setState({
          [name]: value,
        });
      }).catch(() => {
        Choerodon.prompt('查询状态失败，请重试！');
      });
    }
  };

  onDragStart = (result) => {
    const { headerStyle } = this.props;
    const { draggableId } = result;
    const [SwimLaneId, issueId] = draggableId.split(['/']);
    headerStyle.changeStyle(style(SwimLaneId.replace(/[^\w]/g, '')));
    ScrumBoardStore.setIsDragging(SwimLaneId, true);
  };

  onDragEnd = (result) => {
    const { headerStyle } = this.props;
    const { destination, source, draggableId } = result;
    const [SwimLaneId, issueId] = draggableId.split(['/']);
    const allDataMap = ScrumBoardStore.getAllDataMap;
    ScrumBoardStore.resetCanDragOn();
    ScrumBoardStore.setIsDragging(SwimLaneId, false);
    headerStyle.unMountStyle();
    if (!destination) {
      return;
    }

    if (destination.droppableId === source.droppableId && destination.index === source.index) {
      return;
    }

    const [startStatus, startColumn] = source.droppableId.split(['/']);
    const startStatusIndex = source.index;

    const [destinationStatus, destinationColumn] = destination.droppableId.split(['/']);
    const destinationStatusIndex = destination.index;

    const issue = {
      ...allDataMap.get(issueId),
      stayDay: 0,
    };
    const [type, parentId] = SwimLaneId.split('%');

    ScrumBoardStore.updateIssue(issue,
      startStatus,
      startStatusIndex, destinationStatus,
      destinationStatusIndex,
      SwimLaneId).then((data) => {
      if (data.failed) {
        Choerodon.prompt(data.message);
        ScrumBoardStore.setSwimLaneData(SwimLaneId,
          startStatus,
          startStatusIndex,
          SwimLaneId, destinationStatus,
          destinationStatusIndex, issue, true);
      } else {
        if (ScrumBoardStore.getSwimLaneCode === 'parent_child' && parentId !== 'other') {
          ScrumBoardStore.judgeMoveParentToDone(destinationStatus, SwimLaneId, parentId, ScrumBoardStore.getStatusMap.get(destinationStatus).categoryCode === 'done');
        }
        if (data.issueId === ScrumBoardStore.getCurrentClickId) {
          ScrumBoardStore.editRef.current.loadIssueDetail();
        }
        if (startColumn !== destinationColumn) {
          ScrumBoardStore.resetHeaderData(startColumn,
            destinationColumn,
            issue.issueTypeVO.typeCode);
        }
        ScrumBoardStore.rewriteObjNumber(data, issueId, issue);
        // ScrumBoardStore.resetHeaderData(startColumn,destinationColumn)
        if (ScrumBoardStore.needRefresh(issue, destinationStatus)) {
          this.refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
        }
      }
    });
    ScrumBoardStore.setSwimLaneData(SwimLaneId, startStatus, startStatusIndex,
      SwimLaneId, destinationStatus, destinationStatusIndex, issue, false);
  };

  handleToIterationBoard = () => {
    if (!ScrumBoardStore.getSpinIf) {
      to(LINK_URL.iterationBoard(ScrumBoardStore.getSprintId));
    } else {
      Choerodon.prompt('等待加载当前迭代');
    }
  }

  handleModeChange = (e) => {
    if (e.target.value === 'work') {
      this.handleToIterationBoard();
    }
  }

  handleCreateBoardClick = () => {
    ModalPro.open({
      title: '创建看板',
      drawer: true,
      style: {
        width: 380,
      },
      children: <CreateBoard onCreate={this.getBoard} />,
    });
    // 关掉下拉框
    this.SelectBoard.rcSelect.setOpenState(false);
  }

  renderRemainDate = () => (
    <>
      <Icon type="av_timer" style={{ color: 'rgba(0,0,0,0.6)', marginLeft: 10 }} />
      <span style={{
        paddingLeft: 5,
        marginLeft: 0,
        marginRight: 10,
        color: 'rgba(0,0,0,0.6)',
      }}
      >
        {`${ScrumBoardStore.getDayRemain >= 0 ? `${ScrumBoardStore.getDayRemain} days剩余` : '无剩余时间'}`}
      </span>
    </>
  )

  renderSwitchMode = () => (
    <div style={{ marginLeft: 'auto' }}>
      <RadioGroup className="c7nagile-switchRadio" value="board" style={{ marginLeft: 'auto', marginRight: 17 }} onChange={this.handleModeChange}>
        <RadioButton value="board">
          <Tooltip title="看板模式" placement="bottom">
            <Icon type="view_week" />
          </Tooltip>
        </RadioButton>
        <RadioButton value="work">
          <Tooltip title="工作台模式" placement="bottom">
            <Icon type="view_list" />
          </Tooltip>
        </RadioButton>
      </RadioGroup>
    </div>
  )

  refresh = (defaultBoard, url, boardListData) => {
    ScrumBoardStore.setSpinIf(true);
    Promise.all([issueTypeApi.loadAllWithStateMachineId(),
      statusApi.loadAllTransformForAllIssueType(defaultBoard.boardId),
      ScrumBoardStore.axiosGetBoardData(defaultBoard.boardId),
      epicApi.loadEpics()]).then(([issueTypes, stateMachineMap, defaultBoardData, epicData]) => {
      this.dataConverter.setSourceData(epicData, defaultBoardData);
      const renderDataMap = new Map([
        ['parent_child', this.dataConverter.getParentWithSubData],
        ['swimlane_epic', this.dataConverter.getEpicData],
        ['assignee', this.dataConverter.getAssigneeData],
        ['swimlane_none', this.dataConverter.getAllData],
        ['undefined', this.dataConverter.getAssigneeData],
      ]);
      const renderData = renderDataMap.get(defaultBoard.userDefaultBoard)();
      const canDragOn = this.dataConverter.getCanDragOn();
      const statusColumnMap = this.dataConverter.getStatusColumnMap();
      const statusMap = this.dataConverter.getStatusMap();
      const mapStructure = this.dataConverter.getMapStructure();
      const allDataMap = this.dataConverter.getAllDataMap(defaultBoard.userDefaultBoard);
      const headerData = this.dataConverter.getHeaderData();
      ScrumBoardStore.scrumBoardInit(
        AppState,
        url,
        boardListData, defaultBoard, defaultBoardData, null,
        issueTypes, stateMachineMap, canDragOn,
        statusColumnMap, allDataMap, mapStructure, statusMap,
        renderData, headerData,
      );
    });
  }

  handlePriorityChange = (value) => {
    ScrumBoardStore.setPriority(value);
    localPageCacheStore.mergeSetItem('scrumboard', {
      priorityIds: value,
    });
    this.refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
  }

  handleCreateIssue = () => {
    ScrumBoardStore.setCreateIssueVisible(true);
  };

  render() {
    const { HeaderStore } = this.props;
    const {
      updateParentStatus, expandAll,
    } = this.state;
    const menu = AppState.currentMenuType;
    const { type, id: projectId, organizationId: orgId } = menu;
    const currentSprintIsDoing = ScrumBoardStore.didCurrentSprintExist && ScrumBoardStore.sprintNotClosedArray.find((item) => item.statusCode === 'started' && item.sprintId === ScrumBoardStore.sprintId);
    return (
      <Page
        className="c7n-scrumboard-page"
      >
        <Header title="活跃冲刺">
          <Select
            ref={(SelectBoard) => { this.SelectBoard = SelectBoard; }}
            className="SelectTheme  autoWidth"
            value={ScrumBoardStore.getSelectedBoard}
            style={{
              marginRight: 15, fontWeight: 500, lineHeight: '28px',
            }}
            dropdownClassName="c7n-scrumboard-page-select-board-dropdown"
            dropdownStyle={{
              width: 200,
            }}
            onChange={(value) => {
              const selectedBoard = ScrumBoardStore.getBoardList.get(value);
              ScrumBoardStore.setSelectedBoard(value);
              ScrumBoardStore.setSwimLaneCode(selectedBoard.userDefaultBoard);
              this.refresh(selectedBoard);
            }}
            footer={(
              <Permission
                service={['choerodon.code.project.cooperation.iteration-plan.ps.board.create']}
              >
                <Button style={{ width: '100%', height: 42, textAlign: 'left' }} onClick={this.handleCreateBoardClick}>创建看板</Button>
              </Permission>
            )}
          >

            {// ScrumBoardStore.getSpinIf
              [...ScrumBoardStore.getBoardList.values()].map((item) => (
                <Option key={item.boardId} value={item.boardId}>
                  <Tooltip title={item.name}>
                    {item.name}
                  </Tooltip>
                </Option>
              ))
            }
          </Select>
          <HeaderLine />
          <Button onClick={this.handleCreateIssue} icon="playlist_add">创建问题</Button>
          <Button
            className="c7n-scrumboard-settingButton"
            funcType="flat"
            icon="settings"
            onClick={() => {
              to(LINK_URL.scrumboardSetting, {
                params: {
                  boardId: ScrumBoardStore.getSelectedBoard,
                },
              });
            }}
          >
            配置看板
          </Button>
          <ExpandAllButton />
          <ScrumBoardFullScreen />
          {
            currentSprintIsDoing && (
              <>
                {this.renderRemainDate()}
                <Permission
                  service={['choerodon.code.project.cooperation.iteration-plan.ps.sprint.finish']}
                >
                  <Button
                    style={{
                      marginLeft: 15,
                    }}
                    icon="alarm_on"
                    onClick={this.handleFinishSprint}
                  >
                    完成冲刺
                  </Button>
                </Permission>
                {this.renderSwitchMode()}
              </>
            )
          }
        </Header>
        <Breadcrumb />
        <Content style={{
          padding: 0,
          display: 'flex',
          flexDirection: 'column',
          overflow: 'hidden',
          borderTop: '1px solid #D8D8D8',
          paddingTop: 16,
        }}
        >
          <div style={{ display: 'flex' }}>
            <QuickSearch
              onQuickSearchChange={this.onQuickSearchChange}
              onAssigneeChange={this.onAssigneeChange}
              onSprintChange={this.onSprintChange}
              style={{ height: 32, margin: '0 0 16px 16px' }}
            />
            <SelectPriority onChange={this.handlePriorityChange} />
            {ScrumBoardStore.hasSetFilter && <Button type="primary" onClick={this.handleClearFilter}>清除筛选</Button>}
          </div>
          <Spin spinning={ScrumBoardStore.getSpinIf}>
            <div className="c7n-scrumboard" style={HeaderStore.announcementClosed ? {} : { height: 'calc(100vh - 270px)' }}>
              <div style={{ display: 'table', minWidth: '100%' }}>
                <div className="c7n-scrumboard-header">
                  <StatusColumn />
                </div>
                {(!ScrumBoardStore.didCurrentSprintExist
                  || ((!ScrumBoardStore.otherIssue || ScrumBoardStore.otherIssue.length === 0)
                    && (!ScrumBoardStore.interconnectedData
                      || ScrumBoardStore.interconnectedData.size === 0))) ? (
                        <NoneSprint
                          doingSprintExist={ScrumBoardStore.didCurrentSprintExist}
                          hasSetFilter={ScrumBoardStore.hasSetFilter}
                          quickSearchObj={ScrumBoardStore.quickSearchObj}
                        />
                  )
                  : (
                    <div
                      className="c7n-scrumboard-content"
                      style={HeaderStore.announcementClosed ? {} : { height: 'calc(100vh - 272px)' }}
                    >
                      <div className="c7n-scrumboard-container">
                        <SwimLane
                          mode={ScrumBoardStore.getSwimLaneCode}
                          allDataMap={this.dataConverter.getAllDataMap()}
                          mapStructure={ScrumBoardStore.getMapStructure}
                          onDragEnd={this.onDragEnd}
                          onDragStart={this.onDragStart}
                        />
                      </div>
                    </div>
                  )}
              </div>
            </div>

            <IssueDetail
              refresh={this.refresh}
            />
          </Spin>
          <CreateIssue refresh={this.refresh} />
        </Content>
        {
          ScrumBoardStore.getUpdateParent ? (
            <Modal
              closable={false}
              maskClosable={false}
              title="更新父问题"
              visible={ScrumBoardStore.getUpdateParent}
              onCancel={() => {
                ScrumBoardStore.setUpdateParent(false);
              }}
              onOk={() => {
                // 后端要在后续增加的 parentIssues 上加 objVersionNumber
                const data = {
                  issueId: ScrumBoardStore.getUpdatedParentIssue.issueId,
                  objectVersionNumber: ScrumBoardStore.getUpdatedParentIssue.objectVersionNumber,
                  transformId: updateParentStatus || ScrumBoardStore.getTransformToCompleted[0].id,
                };
                issueApi.updateStatus(data.transformId, data.issueId,
                  data.objectVersionNumber).then((res) => {
                  ScrumBoardStore.setUpdateParent(false);
                  this.refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
                }).catch(() => {
                });
              }}
              disableOk={!ScrumBoardStore.getTransformToCompleted.length}
            >
              <p>
                {'任务'}
                {ScrumBoardStore.getUpdatedParentIssue.issueNum}
                {'的全部子任务为done'}
              </p>
              <div style={{ display: 'flex', alignItems: 'center' }}>
                <p style={{ marginRight: 20, marginBottom: 0 }}>您是否要更新父问题进行匹配</p>
                <Select
                  style={{
                    width: 250,
                  }}
                  onChange={(value) => {
                    this.setState({
                      updateParentStatus: value,
                    });
                  }}
                  defaultValue={ScrumBoardStore.getTransformToCompleted.length ? ScrumBoardStore.getTransformToCompleted[0].id : '无'}
                >
                  {
                    ScrumBoardStore.getTransformToCompleted.map((item) => (
                      <Option
                        key={item.id}
                        value={item.id}
                      >
                        {item.statusVO.name}
                      </Option>
                    ))
                  }
                </Select>
              </div>
            </Modal>
          ) : null
        }
      </Page>
    );
  }
}
export default ScrumBoardHome;
