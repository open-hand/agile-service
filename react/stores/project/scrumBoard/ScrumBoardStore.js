/* eslint-disable max-len */
import { createRef } from 'react';
import {
  observable, action, computed, toJS,
} from 'mobx';
import { find, isEmpty, map } from 'lodash';
import { store, stores, Choerodon } from '@choerodon/boot';
import { workCalendarApi, statusApi, boardApi } from '@/api';
import openDescriptionConfirm from '@/components/detail-container/openDescriptionConfirm';

const { AppState } = stores;

@store('ScrumBoardStore')
class ScrumBoardStore {
  editRef = createRef();

  @observable searchVO = {};

  @observable filterManageVisible = false;

  @observable allColumnCount = [];

  @observable calanderCouldUse = false;

  @observable currentSprintExist = true;

  @observable currentDrag = null;

  @observable translateToCompleted = [];

  @observable moveOverRef = {};

  @observable updateParent = false;

  @observable statusMap = new Map();

  @observable headerData = new Map();

  @observable updatedParentIssue = {
    statusId: 0,
    issueTypeId: 0,
  };

  @observable dragStartItem = {};

  @observable dragStart = false;

  @observable swimLaneData = new Map();

  @observable canDragOn = new Map();

  @observable allDataMap = new Map();

  @observable otherIssue = [];

  @observable boardData = [];

  @observable parentIds = [];

  @observable statusCategory = {};

  @observable boardList = new Map();

  @observable selectedBoardId = '';

  @observable unParentIds = [];

  @observable lookupValue = {
    constraint: [],
  };

  @observable stateMachineMap = {

  };

  @observable statusColumnMap = new Map();

  @observable spinIf = true;

  @observable dayRemain = 0;

  @observable sprintId = null;

  @observable sprintName = null;

  @observable interconnectedData = new Map();

  @observable parentId = [];

  @observable mapStructure = {};

  @observable currentConstraint = '';

  @observable currentSprint = {};

  @observable clickIssueDetail = {};

  @observable IssueNumberCount = {};

  @observable assigneer = [];

  @observable swimlaneBasedCode = null;

  @observable quickSearchList = [];

  @observable epicData = [];

  @observable allEpicData = [];

  @observable statusList = [];

  @observable parentIssueIdData = new Set();

  @observable otherQuestionCount = 0;

  @observable workSetting = {
    saturdayWork: false,
    sundayWork: false,
    useHoliday: false,
    timeZoneWorkCalendarDTOS: [],
    workHolidayCalendarDTOS: [],
  };

  @observable workDate = false;

  @observable parentCompleted = [];

  @observable issueTypes = [];

  @observable canAddStatus = true;

  @observable sprintData = true;

  @observable assigneeFilterIds = [];

  @observable isDragging = {
    draggingStart: false,
    draggingSwimlane: 0,
  };

  @observable statusLinkages = [];

  @observable currentBindFunctionMaps = new Map();

  @observable clickIssueItem = {};

  executeBindFunction(keys = [], ...args) {
    const fnResult = [];
    keys.forEach((key) => {
      this.currentBindFunctionMaps.has(key) && fnResult.push(this.currentBindFunctionMaps.get(key)(...args));
    });
    return fnResult;
  }

  @action('绑定一个函数') bindFunction(key, fn) {
    if (typeof (fn) === 'function' && key) {
      this.currentBindFunctionMaps.set(key, fn);
    }
  }

  @action('移除一个绑定函数') removeBindFunction(key) {
    if (typeof (key) === 'string') {
      this.currentBindFunctionMaps.delete(key);
    }
  }

  needRefresh(issue, destinationStatus) {
    if (this.getSwimLaneCode === 'participant' && toJS(issue.participantIds)?.length > 1) {
      return true;
    }
    if ((issue.issueTypeVO.typeCode === 'bug' && issue.relateIssueId) || issue.issueTypeVO.typeCode === 'sub_task' || ['story', 'bug', 'task'].includes(issue.issueTypeVO.typeCode)) {
      const hasLinkage = this.statusLinkages.find((link) => link.issueTypeId === issue.issueTypeVO.id && link.statusId === destinationStatus);
      return hasLinkage;
    }
    return false;
  }

  @action setStatusLinkages(statusLinkages) {
    this.statusLinkages = statusLinkages;
  }

  @computed get getIsDragging() {
    return this.isDragging;
  }

  @action setIsDragging(draggingSwimlane, isDragging) {
    this.isDragging.draggingStart = isDragging;
    this.isDragging.draggingSwimlane = draggingSwimlane;
  }

  @computed get getAssigneeFilterIds() {
    return this.assigneeFilterIds;
  }

  @action setAssigneeFilterIds(data) {
    this.assigneeFilterIds = data;
  }

  @computed get getSprintData() {
    return this.sprintData;
  }

  @action setSprintData(data) {
    this.sprintData = data;
  }

  @computed get getCanAddStatus() {
    return this.canAddStatus;
  }

  @action setCanAddStatus(data) {
    this.canAddStatus = data;
  }

  axiosCanAddStatus() {
    statusApi.checkCanCreateStatus().then((data) => {
      this.setCanAddStatus(data);
    })
      .catch((e) => {
        Choerodon.prompt(e.message);
      });
  }

  @computed get getStatusList() {
    return this.statusList.slice();
  }

  @action setStatusList(data) {
    this.statusList = data;
  }

  @computed get getAllEpicData() {
    return toJS(this.allEpicData);
  }

  @action setBoardParentIssueId(data) {
    data.forEach((item) => item !== 0 && this.parentIssueIdData.add(item));
  }

  @computed get getBoardParentIssueId() {
    return toJS(this.parentIssueIdData);
  }

  // 其他工作项计数 -- 临时逻辑
  @action addOtherQuestionCount() {
    this.otherQuestionCount += 1;
  }

  @action clearOtherQuestionCount() {
    this.otherQuestionCount = 0;
  }

  // 其他工作项计数 -- 临时逻辑
  @computed get getOtherQuestionCount() {
    return this.otherQuestionCount;
  }

  @action setAllEpicData(data) {
    this.allEpicData = data;
  }

  @computed get getEpicData() {
    return toJS(this.epicData);
  }

  @action setEpicData(data) {
    this.epicData = data;
  }

  @computed get getQuickSearchList() {
    return toJS(this.quickSearchList);
  }

  @action setQuickSearchList(data) {
    this.quickSearchList = data;
  }

  @computed get getSwimLaneCode() {
    return this.swimlaneBasedCode;
  }

  @action setSwimLaneCode(data) {
    this.swimlaneBasedCode = data;
  }

  @action setDragStart(data) {
    this.dragStart = data;
  }

  @computed get getDragStart() {
    return this.dragStart;
  }

  @computed get getAssigneer() {
    return toJS(this.assigneer);
  }

  @action setAssigneer(data) {
    this.assigneer = data;
  }

  @computed get getParentId() {
    return toJS(this.parentId);
  }

  @action setParentId(data) {
    this.parentId = data;
  }

  @action setSelectedBoardId(data) {
    this.selectedBoardId = data;
  }

  @observable selectedBoardFilterIds = [];

  @action setSelectedBoardFilterIds(data) {
    this.selectedBoardFilterIds = data;
  }

  @action initBoardList(boardListData) {
    if (boardListData) {
      this.boardList = observable.map(boardListData.map((board) => [board.boardId, board]));
    }
  }

  clickIssueMap = observable.map();

  @action setClickedIssue(issueId) {
    const setData = () => {
      if (!this.clickIssueMap.has(issueId)) {
        this.clickIssueMap.clear();
        this.clickIssueMap.set(issueId, true);
      }
    };

    if (!this.detailProps.descriptionChanged) {
      setData();
    } else {
      openDescriptionConfirm({
        onOk: () => {
          setData();
          if (this.detailProps.setDescriptionChanged) {
            this.detailProps.setDescriptionChanged(false);
          }
        },
      });
    }
  }

  @action resetClickedIssue() {
    this.clickIssueMap.clear();
  }

  @computed get getCurrentClickId() {
    return [...this.clickIssueMap.keys()][0];
  }

  @observable detailProps = {};

  @action setDetailProps = (data) => {
    this.detailProps = data;
  }

  @action setMoveOverRef(data) {
    this.moveOverRef = data;
  }

  @computed get getMoveOverRef() {
    return this.moveOverRef;
  }

  @action judgeMoveParentToDone(destinationStatus, swimLaneId, parentId, statusIsDone) {
    const completedStatusIssueLength = Object.keys(this.swimLaneData[swimLaneId])
      .filter((statusId) => this.statusMap.get(statusId).completed === true)
      .map((statusId) => this.swimLaneData[swimLaneId][statusId].length)
      .reduce((accumulator, currentValue) => accumulator + currentValue);
    if (statusIsDone && completedStatusIssueLength === this.interconnectedData.get(parentId).subIssueData.length && this.interconnectedData.get(parentId).categoryCode !== 'done') {
      this.updatedParentIssue = this.interconnectedData.get(parentId);
      this.setTransFromData(this.updatedParentIssue, parentId);
    } else {
      this.interconnectedData.set(parentId, {
        ...this.interconnectedData.get(parentId),
        canMoveToComplish: false,
      });
    }
  }

  @action setSearchVO(data) {
    this.searchVO = data;
  }

  @action setFilterManageVisible(data) {
    this.filterManageVisible = data;
  }

  @computed get getFilterManageVisible() {
    return this.filterManageVisible;
  }

  @action clearFilter() {
    this.searchVO = {};
  }

  setTransFromData(parentIssue, parentId) {
    statusApi.loadTransformStatusByIssue(parentIssue.statusId,
      parentIssue.issueId, parentIssue.issueTypeId).then(
      action('fetchSuccess', (res) => {
        this.updatedParentIssue = parentIssue;
        this.translateToCompleted = res.filter((transform) => transform.statusVO.type === 'done');
        this.updateParent = true;
      }),
    );
  }

  @computed get getTransformToCompleted() {
    return this.translateToCompleted;
  }

  @computed get getUpdatedParentIssue() {
    return this.updatedParentIssue;
  }

  @action updateStatusLocal(columnId, data, res) {
    const status = this.findStatusById(columnId, data.statusId);
    status.completed = res.completed;
    status.objectVersionNumber = res.objectVersionNumber;
  }

  findStatusById(columnId, statusId) {
    const data = this.boardData;
    const column = find(data, { columnId });
    const status = find(column.subStatusDTOS, { statusId });
    return status;
  }

  @computed get getIssueNumberCount() {
    return toJS(this.IssueNumberCount);
  }

  @action updateParentIssueToDone(issueId, data) {
    this.interconnectedData.set(issueId, {
      ...this.updatedParentIssue,
      ...data,
    });
  }

  @action setIssueNumberCount(data) {
    this.IssueNumberCount = data;
  }

  @computed get getClickIssueDetail() {
    return this.clickIssueDetail;
  }

  /**
   *
   * @param {*} parentIssueId
   * @param {*} isSkipIssue  是否为跳转工作项
   */
  @action resetCurrentClick(parentIssueId, isSkipIssue = false) {
    if (this.currentClickTarget && !isSkipIssue) {
      this.currentClickTarget.style.backgroundColor = '#fff';
    }
    this.currentClickTarget = null;
    this.currentClick = parentIssueId;
    if (this.allDataMap.get(parentIssueId)) {
      this.clickIssueDetail = this.allDataMap.get(parentIssueId);
    }
    // this.clickIssueDetail = this.allDataMap.get(parentIssueId);
  }

  @action resetDataBeforeUnmount() {
    this.spinIf = true;
    this.swimLaneData = null;
    this.interconnectedData = new Map();
    this.statusColumnMap = new Map();
    this.allDataMap = new Map();
    this.boardData = [];
    this.statusMap = new Map();
    this.otherIssue = [];
    this.selectSprint = undefined;
    this.sprintNotClosedArray = [];
    this.headerData = new Map();
    this.clearFilter();
    this.currentSprintExist = false;
    this.calanderCouldUse = false;
    this.clickIssueMap.clear();
    this.currentBindFunctionMaps.clear();
  }

  @computed get getDayRemain() {
    return toJS(this.dayRemain);
  }

  @action setCurrentSprint(data) {
    this.currentSprint = data;
  }

  @computed get getSprintId() {
    return this.sprintId;
  }

  @computed get getSprintName() {
    return this.sprintName;
  }

  @observable selectSprint = undefined;

  @action setSelectSprint = (data) => {
    this.selectSprint = data;
  }

  @observable sprintNotClosedArray = [];

  @action setSprintNotClosedArray = (sprintNotClosedArray) => {
    this.sprintNotClosedArray = sprintNotClosedArray;
  }

  @computed get getCurrentConstraint() {
    return this.currentConstraint;
  }

  @observable isHasFilter = false;

  @action setIsHasFilter(data) {
    this.isHasFilter = data;
  }

  @computed get hasSetFilter() {
    return this.isHasFilter;
  }

  @action setCurrentConstraint(data) {
    this.currentConstraint = data;
  }

  @computed get getLookupValue() {
    return toJS(this.lookupValue);
  }

  @action setLookupValue(data) {
    this.lookupValue = data;
  }

  @computed get getUnParentIds() {
    return toJS(this.unParentIds);
  }

  @action setUnParentIds(data) {
    this.unParentIds = data;
  }

  @computed get getSelectedBoard() {
    return this.selectedBoardId;
  }

  @action setSelectedBoard(data) {
    this.currentSprintExist = false;
    this.selectedBoardId = data;
  }

  @computed get getBoardList() {
    return this.boardList;
  }

  @action setBoardList(key, data) {
    this.boardList.set(key, data);
  }

  @action rewriteCurrentConstraint({ columnConstraint, objectVersionNumber }, boardId, boardData) {
    this.currentConstraint = columnConstraint;
    this.boardList.set(boardId, {
      ...boardData,
      columnConstraint,
      objectVersionNumber,
    });
  }

  @action setBoardListData(boardListData, { boardId, userDefaultBoard, columnConstraint }) {
    this.boardList = boardListData;
    this.selectedBoardId = boardId;
    this.swimlaneBasedCode = userDefaultBoard;
    this.currentConstraint = columnConstraint;
  }

  @computed get getStatusCategory() {
    return toJS(this.statusCategory);
  }

  @action setStatusCategory(data) {
    this.statusCategory = data;
  }

  @computed get getParentIds() {
    return toJS(this.parentIds);
  }

  @action setParentIds(data) {
    this.parentIds = data;
  }

  @computed get getParentCompleted() {
    return toJS(this.parentCompleted);
  }

  @action setParentCompleted(data) {
    this.parentCompleted = data;
  }

  @computed get getBoardData() {
    return toJS(this.boardData);
  }

  getColumnDeleteAble(columnId) {
    const { boardData } = this;
    const otherColumn = boardData.filter((c) => c.columnId !== '0' && c.columnId !== columnId);
    return otherColumn.some((c) => c.subStatusDTOS && c.subStatusDTOS.length > 0);
  }

  @action setBoardData(data) {
    this.boardData = data;
  }

  @observable assigneeProps = [];

  @computed get getAssigneeProps() {
    return this.assigneeProps;
  }

  @action setAssigneeProps(data) {
    this.assigneeProps = data;
  }

  @computed get getCalanderCouldUse() {
    return this.calanderCouldUse;
  }

  axiosGetBoardData(boardId) {
    // if (!this.searchVO.otherArgs || isEmpty(this.searchVO.otherArgs.sprint)) {
    //   this.getSprintId && set(this.searchVO, 'otherArgs.sprint', [this.getSprintId]);
    // }
    return boardApi.load(boardId, this.searchVO);
  }

  updateIssue = (
    {
      issueId, objectVersionNumber, boardId, originColumnId, columnId,
      before, sprintId, rank, issueTypeId,
    }, startStatus, startStatusIndex, destinationStatus, destinationStatusIndex, SwimLaneId,
  ) => {
    const proId = AppState.currentMenuType.id;
    let outsetIssueId = '';

    if (destinationStatusIndex !== 0) {
      // 从另一个列拖过来传，目标位置-1的id
      if (startStatus !== destinationStatus) {
        outsetIssueId = this.swimLaneData[SwimLaneId][destinationStatus][destinationStatusIndex - 1].issueId;
        // 从同一列的前面拖过来传，目标位置的id
      } else if (startStatusIndex < destinationStatusIndex) {
        outsetIssueId = this.swimLaneData[SwimLaneId][destinationStatus][destinationStatusIndex].issueId;
        // 从同一列的后面拖过来传，目标位置-1的id
      } else if (startStatusIndex > destinationStatusIndex) {
        outsetIssueId = this.swimLaneData[SwimLaneId][destinationStatus][destinationStatusIndex - 1].issueId;
      }
    }
    const data = {
      issueId,
      objectVersionNumber,
      statusId: destinationStatus,
      boardId: this.selectedBoardId,
      originColumnId: this.statusColumnMap.get(startStatus),
      columnId: this.statusColumnMap.get(destinationStatus),
      before: destinationStatusIndex === 0,
      outsetIssueId,
      sprintId: this.sprintId,
      rankFlag: true,
    };
    // Object.keys(this.stateMachineMap)[0] 若无工作项类型状态机方案，则选用默认的
    const { id: transformId } = this.stateMachineMap[issueTypeId] ? this.stateMachineMap[issueTypeId][startStatus].find((issue) => issue.endStatusId === destinationStatus) : this.stateMachineMap[Object.keys(this.stateMachineMap)[0]][startStatus].find((issue) => issue.endStatusId === destinationStatus);
    return boardApi.moveIssue(issueId, transformId, data);
  };

  @computed get getDragStartItem() {
    return this.dragStartItem;
  }

  @action setDragStartItem(data) {
    this.dragStartItem = data;
  }

  @action setWorkSetting(data) {
    this.workSetting = data;
  }

  @computed get getWorkSetting() {
    return this.workSetting;
  }

  // 查询组织层工作日历设置
  axiosGetWorkSetting(year) {
    return workCalendarApi.getWorkSetting(year).then((data) => {
      if (data) {
        this.setWorkSetting(data);
      }
    });
  }

  @action setWorkDate(data) {
    if (data.sprintId) {
      this.calanderCouldUse = true;
    }
    this.workDate = data;
  }

  @computed get getWorkDate() {
    return this.workDate;
  }

  // 获取项目层工作日历
  axiosGetCalendarData = (year) => {
    workCalendarApi.getCalendar(year).then((data) => {
      if (data) {
        this.setWorkDate(data);
      } else {
        this.setWorkDate(false);
      }
    }).catch(() => {
      this.setWorkDate(false);
    });
  };

  @computed get getIssueTypes() {
    return toJS(this.issueTypes);
  }

  @action setIssueTypes(data) {
    this.issueTypes = data;
  }

  loadStatus = () => {
    statusApi.loadByProject().then((data) => {
      if (data && !data.failed) {
        this.setStatusList(data);
      } else {
        this.setStatusList([]);
      }
    }).catch(() => {
      this.setStatusList([]);
    });
  };

  @action setSpinIf(data) {
    // this.currentSprintExist = false;
    this.spinIf = data;
  }

  @computed get getSpinIf() {
    return this.spinIf;
  }

  @action scrumBoardInit(AppStates, url = null, boardListData = null, { boardId, userDefaultBoard, columnConstraint }, { currentSprint, allColumnNum }, quickSearchList, issueTypes, stateMachineMap, canDragOn, statusColumnMap, allDataMap, mapStructure, statusMap, renderData, headerData) {
    this.boardData = [];
    this.spinIf = false;
    this.quickSearchList = [];
    this.sprintData = false;
    this.assigneer = [];
    this.parentIds = [];
    this.epicData = [];
    if (boardListData) {
      this.boardList = observable.map(boardListData.map((board) => [board.boardId, board]));
    }
    this.selectedBoardId = boardId;
    this.swimlaneBasedCode = userDefaultBoard;
    this.currentConstraint = columnConstraint;
    this.quickSearchList = quickSearchList;
    this.allColumnCount = observable.map(allColumnNum.map(({ columnId, issueCount }) => [columnId, issueCount]));
    if (currentSprint) {
      this.currentSprintExist = Object.values(currentSprint).filter(Boolean).length > 0;
      this.dayRemain = currentSprint.dayRemain;
      this.sprintId = currentSprint.sprintId;
      this.sprintName = currentSprint.sprintName;
    } else {
      this.currentSprintExist = false;
    }
    this.allDataMap = allDataMap;
    this.mapStructure = mapStructure;
    if (url && url.paramIssueId) {
      this.clickIssueDetail = { issueId: url.paramIssueId };
    }
    if (issueTypes && !issueTypes.failed) {
      this.issueTypes = issueTypes;
    } else {
      this.issueTypes = [];
    }
    this.stateMachineMap = stateMachineMap;
    const oldData = toJS(this.canDragOn) ?? {};
    this.canDragOn = observable.map(canDragOn);
    // 处理子任务设置了状态联动并在拖动后，立马拖动父级的工作项
    Object.keys(oldData).forEach((status) => {
      if (this.canDragOn.has(status)) {
        this.canDragOn.set(status, oldData[status]);
      }
    });
    this.statusColumnMap = statusColumnMap;
    const { unInterConnectedDataMap, interConnectedDataMap, swimLaneData } = renderData;
    this.otherIssue = unInterConnectedDataMap;
    this.interconnectedData = observable.map(interConnectedDataMap);
    this.swimLaneData = swimLaneData;
    this.statusMap = observable.map(statusMap);
    this.headerData = observable.map(headerData);
  }

  @computed get getAllColumnCount() {
    return this.allColumnCount;
  }

  @computed get getHeaderData() {
    return this.headerData;
  }

  @action resetHeaderData(startColumnId, destinationColumnId, issueType) {
    const startColumnData = this.headerData.get(startColumnId);
    const destinationColumnData = this.headerData.get(destinationColumnId);
    this.headerData.set(startColumnId, {
      ...startColumnData,
      columnIssueCount: startColumnData.columnIssueCount - 1,
    });
    this.headerData.set(destinationColumnId, {
      ...destinationColumnData,
      columnIssueCount: destinationColumnData.columnIssueCount + 1,
    });
    if (this.getAllColumnCount.size > 0) {
      this.setColumnConstrint(startColumnId, destinationColumnId, issueType);
    }
  }

  @action setColumnConstrint(startColumnId, destinationColumnId, issueType) {
    const startColumnCount = this.allColumnCount.get(startColumnId);
    const destinationColumnCount = this.allColumnCount.get(destinationColumnId);
    if ((this.currentConstraint === 'issue_without_sub_task' && issueType !== 'sub_task') || this.currentConstraint === 'issue') {
      this.allColumnCount.set(startColumnId, startColumnCount - 1);
      this.allColumnCount.set(destinationColumnId, destinationColumnCount + 1);
    }
  }

  @computed get getStateMachineMap() {
    return this.stateMachineMap;
  }

  @computed get getStatusMap() {
    return this.statusMap;
  }

  @computed get getMapStructure() {
    return this.mapStructure;
  }

  @computed get getCurrentDrag() {
    return this.currentDrag;
  }

  @action setCurrentDrag(data) {
    this.currentDrag = data;
  }

  @action setWhichCanNotDragOn(statusId, { id: typeId }) {
    [...this.canDragOn.keys()].forEach((status) => {
      if (this.stateMachineMap[typeId]) {
        if (this.stateMachineMap[typeId][statusId].find((issue) => issue.endStatusId === status)) {
          this.canDragOn.set(status, false);
        } else {
          this.canDragOn.set(status, true);
        }
        // Object.keys(this.stateMachineMap)[0] 若无工作项类型状态机方案，则选用默认的
      } else if (this.stateMachineMap[Object.keys(this.stateMachineMap)[0]][statusId].find((issue) => issue.endStatusId === status)) {
        this.canDragOn.set(status, false);
      } else {
        this.canDragOn.set(status, true);
      }
    });
  }

  @computed get getClickIssueItem() {
    return this.clickIssueItem;
  }

  @action setClickIssueItem(data) {
    this.clickIssueItem = data;
  }

  @action checkAndSetCanDragOn(statusId) {
    !this.canDragOn.get(statusId) && this.canDragOn.set(statusId, true);
  }

  checkCanDragOn = async (issueId) => {
    try {
      const check = await boardApi.getNotAllowedTransferStatus(issueId);
      if (!isEmpty(check)) {
        check.forEach((status) => {
          status?.id && this.checkAndSetCanDragOn(status.id);
        });
      }
    } catch (e) {
      // return;
    }
  };

  @computed get getCanDragOn() {
    return this.canDragOn;
  }

  @computed get getCanDragOnToJS() {
    return toJS(this.canDragOn);
  }

  @computed get getIssueWithStatus() {
    return toJS(this.interconnectedData);
  }

  @action setOtherQuestion({ parentDataMap }) {
    // 数组 单独管理
    this.otherIssue = parentDataMap;
  }

  @computed get getOtherQuestion() {
    return this.otherIssue;
  }

  @action setInterconnectedData(data) {
    // id - Map 相关联
    this.interconnectedData = data;
  }

  @computed get getInterconnectedData() {
    return this.interconnectedData;
  }

  @computed get didCurrentSprintExist() {
    return this.currentSprintExist;
  }

  @action resetCurrentSprintExist() {
    this.currentSprintExist = null;
  }

  @action resetCanDragOn() {
    [...this.canDragOn.keys()].forEach((status) => {
      this.canDragOn.set(status, false);
    });
  }

  @action setSwimLaneData(startSwimLane, startStatus, startStatusIndex, destinationSwimLane, destinationStatus, destinationStatusIndex, issue, revert) {
    if (!revert) {
      this.swimLaneData[startSwimLane][startStatus].splice(startStatusIndex, 1);
      this.swimLaneData[startSwimLane][destinationStatus].splice(destinationStatusIndex, 0, {
        ...issue,
        statusId: destinationStatus,
      });
    } else {
      this.swimLaneData[startSwimLane][destinationStatus].splice(destinationStatusIndex, 1);
      this.swimLaneData[startSwimLane][startStatus].splice(startStatusIndex, 0, {
        ...issue,
        statusId: startStatus,
      });
    }
  }

  @action rewriteObjNumber({ objectVersionNumber }, issueId, issue) {
    this.allDataMap.set(issueId, {
      ...issue,
      objectVersionNumber,
    });
  }

  @computed get getSwimLaneData() {
    // swimLaneCode - Map 相关联
    return this.swimLaneData;
  }

  @computed get getAllDataMap() {
    return this.allDataMap;
  }

  @action setUpdateParent(data) {
    this.updateParent = data;
  }

  @computed get getUpdateParent() {
    return this.updateParent;
  }

  @action setEditRef(ref) {
    this.editRef = ref;
  }

  axiosGetSelectedBoardFilterIds = async () => {
    const res = await boardApi.getFilterSelected(this.selectedBoardId);
    this.setSelectedBoardFilterIds(map(res, 'quickFilterId'));
  };
}

const scrumBoardStore = new ScrumBoardStore();
export default scrumBoardStore;
