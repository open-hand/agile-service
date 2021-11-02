import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import {
  Page, Header, Content, stores, Breadcrumb, Choerodon, WSHandler,
} from '@choerodon/boot';
import {
  Select, Icon, Modal, Form,
} from 'choerodon-ui';
import screenfull from 'screenfull';
import { HeaderButtons } from '@choerodon/master';
import { cloneDeep, set } from 'lodash';
import { Modal as ModalPro } from 'choerodon-ui/pro';
import queryString from 'query-string';
import { LoadingHiddenWrap, LoadingProvider } from '@/components/Loading';
import CloseSprint from '@/components/close-sprint';
import {
  sprintApi, issueApi, epicApi, issueTypeApi, statusApi, boardApi,
} from '@/api';
import LINK_URL from '@/constants/LINK_URL';
import to from '@/utils/to';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';

import FilterManage from '@/components/FilterManage';
import openCreateIssue from '@/components/create-issue';
import ScrumBoardDataController from './ScrumBoardDataController';
import ScrumBoardStore from '../../../stores/project/scrumBoard/ScrumBoardStore';
import StatusColumn from '../ScrumBoardComponent/StatusColumn/StatusColumn';
import './ScrumBoardHome.less';
import IssueDetail from '../ScrumBoardComponent/IssueDetail/IssueDetail';
import NoneSprint from '../ScrumBoardComponent/NoneSprint/NoneSprint';
import '../ScrumBoardComponent/RenderSwimLaneContext/RenderSwimLaneContext.less';
import SwimLane from '../ScrumBoardComponent/RenderSwimLaneContext/SwimLane';
import CSSBlackMagic from '../../../components/CSSBlackMagic/CSSBlackMagic';
import ScrumBoardFullScreen from '../ScrumBoardComponent/ScrumBoardFullScreen';
import CreateBoard from '../ScrumBoardComponent/CreateBoard';
import ExpandAllButton from '../ScrumBoardComponent/expand-all-button';
import BoardSearch from '../ScrumBoardComponent/board-search';
import SelectBoard from '../ScrumBoardComponent/select-board';
import StatusLinkageWSHandle from '@/components/StatusLinkageWSHandle';

const { AppState } = stores;

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
    this.issueSearchStore = null;
    this.state = {
      updateParentStatus: null,
    };
  }

  componentDidMount() {
    ScrumBoardStore.setSelectedBoardId('');
    const defaultSearchVO = cloneDeep(localPageCacheStore.getItem('scrumBoard.searchVO') || {});
    ScrumBoardStore.bindFunction('refresh', (sprintId) => {
      const currentSprint = sprintId ?? defaultSearchVO?.otherArgs?.sprint;
      if (!defaultSearchVO.otherArgs || !defaultSearchVO.otherArgs.sprint || defaultSearchVO.otherArgs.sprint.length === 0) {
        // defaultSearchVO.otherArgs.sprint = [sprintId];
        sprintId && set(defaultSearchVO, 'otherArgs.sprint', [sprintId]);
      }
      ScrumBoardStore.setSearchVO(defaultSearchVO);
      this.getBoard(!currentSprint);
    });
    // eslint-disable-next-line react/destructuring-assignment
    const { state } = this.props.location;
    const { paramIssueId } = queryString.parse(this.props.location.search);
    if (paramIssueId || (state && state.issueId)) {
      ScrumBoardStore.setClickedIssue(paramIssueId || state.issueId);
    }
  }

  componentWillUnmount() {
    this.dataConverter = null;
    ScrumBoardStore.resetDataBeforeUnmount();
    // 退出全屏
    screenfull.exit();
    document.body.classList.remove('c7n-scrumboard-fullScreen');
  }

  getBoard = async (noRefresh) => {
    const { location } = this.props;
    const url = this.paramConverter(location.search);
    const boardListData = await boardApi.loadAll();
    const statusLinkages = await boardApi.getStatusLinkages();
    ScrumBoardStore.initBoardList(boardListData);
    ScrumBoardStore.setStatusLinkages(statusLinkages);
    const defaultBoard = boardListData.find((item) => item.userDefault) || boardListData[0];
    if (defaultBoard.boardId) {
      ScrumBoardStore.setSelectedBoardId(defaultBoard.boardId);
      noRefresh ? ScrumBoardStore.setSpinIf(false) : this.refresh(defaultBoard, url, boardListData);
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

  handleClearFilter = () => {
    ScrumBoardStore.clearFilter();
    localPageCacheStore.remove('scrumboard');
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
    const sprintInfo = completeMessage.sprintNames[0];
    const defaultValuePrompt = undefined; // (sprintId) ? `提示：冲刺${sprintInfo.sprintName}是默认选项，完成后冲刺字段默认值将清空` : undefined;
    CloseSprint({
      completeMessage,
      defaultValuePrompt,
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
    // ScrumBoardStore.resetCanDragOn();
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
    }).catch((error) => {
      console.error(error);
      ScrumBoardStore.setSwimLaneData(SwimLaneId,
        startStatus,
        startStatusIndex,
        SwimLaneId, destinationStatus,
        destinationStatusIndex, issue, true);
    });
    ScrumBoardStore.setSwimLaneData(SwimLaneId, startStatus, startStatusIndex,
      SwimLaneId, destinationStatus, destinationStatusIndex, issue, false);
  };

  handleCreateBoardClick = () => {
    ModalPro.open({
      title: '创建看板',
      drawer: true,
      style: {
        width: 380,
      },
      children: <CreateBoard onCreate={this.getBoard} />,
    });
  }

  renderRemainDate = () => (
    <div style={{ display: 'flex', alignItems: 'center' }}>
      <Icon type="av_timer" style={{ color: '#0F1358' }} />
      <span style={{
        paddingLeft: 5,
        marginLeft: 0,
        color: '#0F1358',
      }}
      >
        {`${ScrumBoardStore.getDayRemain >= 0 ? `${ScrumBoardStore.getDayRemain} days剩余` : '无剩余时间'}`}
      </span>
    </div>
  )

  refresh = (defaultBoard, url, boardListData) => {
    if (!defaultBoard) {
      return;
    }
    ScrumBoardStore.setSpinIf(true);
    Promise.all([issueTypeApi.loadAllWithStateMachineId(),
      statusApi.loadAllTransformForAllIssueType(defaultBoard.boardId),
      ScrumBoardStore.axiosGetBoardData(defaultBoard.boardId),
      epicApi.loadEpics()]).then(([issueTypes, stateMachineMap, defaultBoardData, epicData]) => {
      if (!this.dataConverter) {
        return;
      }
      this.dataConverter.setSourceData(epicData, defaultBoardData);
      const renderDataMap = new Map([
        ['parent_child', this.dataConverter.getParentWithSubData],
        ['swimlane_epic', this.dataConverter.getEpicData],
        ['assignee', this.dataConverter.getAssigneeData],
        ['participant', this.dataConverter.getParticipantData],
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

  handleFilterChange = () => {
    this.refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
  }

  handleCreateIssue = () => {
    const doingSprint = ScrumBoardStore.didCurrentSprintExist ? ScrumBoardStore.sprintNotClosedArray.find((item) => item.statusCode === 'started') : {};
    openCreateIssue({
      defaultValues: {
        sprint: ScrumBoardStore.quickSearchObj?.sprintId || doingSprint?.sprintId,
      },
      originFrom: 'scrumBoard',
      onCreate: (res) => {
        const { sprintId } = res.activeSprint || {};
        if (ScrumBoardStore.getSprintId && String(sprintId) === String(ScrumBoardStore.getSprintId)) {
          this.refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
        }
      },
    });
  };

  handleSaveSearchStore = (data) => {
    this.issueSearchStore = data;
  }

  handleClickFilterManage = () => {
    const filterManageVisible = ScrumBoardStore.getFilterManageVisible;
    ScrumBoardStore.setFilterManageVisible(!filterManageVisible);
  };

  render() {
    const {
      updateParentStatus,
    } = this.state;
    const currentSprintIsDoing = ScrumBoardStore.didCurrentSprintExist && ScrumBoardStore.sprintNotClosedArray.find((item) => item.statusCode === 'started' && item.sprintId === ScrumBoardStore.sprintId);
    return (
      <Page
        className="c7n-scrumboard-page"
      >
        <Header title="活跃冲刺">
          <SelectBoard
            style={{ marginRight: 8 }}
            onFooterClick={this.handleCreateBoardClick}
            onChange={(value) => {
              const selectedBoard = ScrumBoardStore.getBoardList.get(value);
              ScrumBoardStore.setSelectedBoard(value);
              ScrumBoardStore.setSwimLaneCode(selectedBoard.userDefaultBoard);
              this.refresh(selectedBoard);
            }}
          />
          <HeaderButtons
            items={[
              {
                name: '创建工作项',
                icon: 'playlist_add',
                handler: this.handleCreateIssue,
                display: true,
              }, {
                display: true,
                element: <ExpandAllButton />,
              }, {
                display: true,
                name: '更多操作',
                groupBtnItems: [{
                  name: '配置看板',
                  handler: () => {
                    to(LINK_URL.scrumboardSetting, {
                      params: {
                        boardId: ScrumBoardStore.getSelectedBoard,
                      },
                    });
                  },
                }, {
                  name: '个人筛选',
                  handler: this.handleClickFilterManage,
                }],
              },
              {
                display: true,
                element: <ScrumBoardFullScreen />,
              },
              {
                display: true,
                icon: 'refresh',
                // funcType: 'flat',
                handler: () => this.refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard)),
              },
              {
                name: '完成冲刺',
                icon: 'alarm_on',
                // funcType: 'flat',
                handler: this.handleFinishSprint,
                display: !!currentSprintIsDoing,
                permissions: ['choerodon.code.project.cooperation.iteration-plan.ps.sprint.finish'],
                preElement: this.renderRemainDate(),
              },

            ]}
          />
        </Header>
        <Breadcrumb />
        <Content
          className="c7n-scrumboard-content"
          style={{
            padding: 0,
            display: 'flex',
            overflow: 'hidden',
            borderTop: '1px solid #D8D8D8',
            paddingTop: 16,
          }}
        >
          <LoadingProvider
            loading={ScrumBoardStore.getSpinIf}
            loadingStyle={{ position: 'relative' }}
            style={{
              flex: 1, overflow: 'hidden', display: 'flex', flexDirection: 'column',
            }}
          >

            <BoardSearch onRefresh={this.handleFilterChange} saveStore={this.handleSaveSearchStore} />
            <div className="c7n-scrumboard">
              <div style={{ display: 'table', minWidth: '100%' }}>
                {(!ScrumBoardStore.didCurrentSprintExist
                  || ((!ScrumBoardStore.otherIssue || ScrumBoardStore.otherIssue.length === 0)
                    && (!ScrumBoardStore.interconnectedData
                      || ScrumBoardStore.interconnectedData.size === 0))) ? (
                        <LoadingHiddenWrap>
                          <NoneSprint
                            doingSprintExist={ScrumBoardStore.didCurrentSprintExist}
                            hasSetFilter={this.issueSearchStore?.isHasFilter}
                            filterItems={this.issueSearchStore?.currentFlatFilter ?? {}}
                          />
                        </LoadingHiddenWrap>
                  )
                  : (
                    <>
                      <div className="c7n-scrumboard-header">
                        <StatusColumn />
                      </div>
                      <div
                        className="c7n-scrumboard-content"
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
                    </>
                  )}
              </div>
            </div>
            <IssueDetail
              refresh={this.refresh}
            />
            {this.issueSearchStore ? (
              <FilterManage
                visible={ScrumBoardStore.getFilterManageVisible}
                setVisible={() => ScrumBoardStore.setFilterManageVisible(!ScrumBoardStore.getFilterManageVisible)}
                issueSearchStore={this.issueSearchStore}
              />
            ) : null}
          </LoadingProvider>
        </Content>
        {
          ScrumBoardStore.getUpdateParent ? (
            <Modal
              closable={false}
              maskClosable={false}
              title="更新父工作项"
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
                }).catch((err) => {
                  if (err.code === 'error.stateMachine.executeTransform') {
                    Choerodon.prompt('该工作项状态已被修改，请打开父工作项进行状态修改', 'error');
                  }
                });
              }}
              disableOk={!ScrumBoardStore.getTransformToCompleted.length}
            >
              <p>
                {'任务'}
                {ScrumBoardStore.getUpdatedParentIssue?.issueNum}
                {'全部子任务均为已解决状态'}
              </p>
              <div style={{ display: 'flex', alignItems: 'center' }}>
                <p style={{ marginRight: 20, marginBottom: 0 }}>是否更新父级工作项状态</p>
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
                      <Select.Option
                        key={item.id}
                        value={item.id}
                      >
                        {item.statusVO.name}
                      </Select.Option>
                    ))
                  }
                </Select>
              </div>
            </Modal>
          ) : null
        }
        <StatusLinkageWSHandle />
      </Page>
    );
  }
}
export default ScrumBoardHome;
