import React from 'react';
import {
  observable, action, computed, toJS,
} from 'mobx';
import {
  sortBy, find, uniq, intersection, pick, isObject,
} from 'lodash';
import { store } from '@choerodon/boot';
import { Modal, DataSet } from 'choerodon-ui/pro';
import Moment from 'moment';
import { extendMoment } from 'moment-range';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import {
  featureApi, sprintApi, piApi, storyMapApi, epicApi, priorityApi, issueTypeApi, commonApi, versionApi, quickFilterApi, issueApiConfig,
} from '@/api';
import { getProjectId } from '@/utils/common';
import { isInProgram } from '@/utils/program';
import openDescriptionConfirm from '@/components/detail-container/openDescriptionConfirm';

const moment = extendMoment(Moment);
function randomItem(array) {
  const index = Math.floor(Math.random() * (array.length - 1));
  return array[index];
}
@store('BacklogStore')
class BacklogStore {
  @observable createdSprint = '';

  @observable hasActiveSprint = false;

  @observable issueCantDrag = false;

  @observable epicFilter = {};

  @observable versionFilter = {};

  @observable filter = { advancedSearchArgs: {} };

  @observable filterSelected = false;

  @observable epicList = [];

  @observable featureList = [];

  @observable selectedPiId = undefined;

  @observable selectedSprintId = undefined;

  @observable selectedIssueId = [];

  @observable issueMap = observable.map();

  @observable currentDrag = observable.map();

  @observable showRealQuickSearch = true;

  @observable ctrlClicked = false;

  @observable shiftClicked = false;

  @observable multiSelected = observable.map();

  @observable prevClickedIssue = null;

  @observable spinIf = false;

  @observable whichVisible = '';

  @observable sprintData = [];

  @observable versionData = [];

  @observable epicData = [];

  @observable chosenVersion = 'all';

  @observable chosenEpic = 'all';

  @observable chosenFeature = 'all';

  @observable onlyMe = false;

  @observable recent = false;

  @observable isDragging = '';

  @observable isLeaveSprint = false;

  @observable clickIssueDetail = {};

  @observable clickIssueId = null;

  @observable sprintWidth;

  @observable colorLookupValue = [];

  @observable quickFilters = [];

  @observable projectInfo = {};

  @observable selectIssues = [];

  @observable issueTypes = [];

  @observable defaultPriority = false;

  @observable cleanQuickSearch = false;

  @observable newIssueVisible = false;

  @observable isInProgramData = {};

  @computed get getIsInProgramData() {
    return this.isInProgramData;
  }

  @action setIsInProgramData(data) {
    this.isInProgramData = data;
  }

  @computed get getNewIssueVisible() {
    return this.newIssueVisible;
  }

  @action setNewIssueVisible(data) {
    this.newIssueVisible = data;
  }

  @computed get getSelectIssue() {
    return this.selectIssues;
  }

  @action setSelectIssue(data) {
    this.selectIssues = data;
  }

  @computed get getProjectInfo() {
    return toJS(this.projectInfo);
  }

  @action setProjectInfo(data) {
    this.projectInfo = data;
  }

  @computed get getQuickFilters() {
    return toJS(this.quickFilters);
  }

  @action setQuickSearchClean(data) {
    this.cleanQuickSearch = data;
  }

  @computed get getQuickSearchClean() {
    return toJS(this.cleanQuickSearch);
  }

  @observable assigneeProps = [];

  @computed get getAssigneeProps() {
    return this.assigneeProps;
  }

  @action setAssigneeProps(data) {
    this.assigneeProps = data;
  }

  getSprintFilter() {
    const data = {
      advancedSearchArgs: {},
    };
    if (this.chosenEpic !== 'all') {
      if (this.chosenEpic === 'unset') {
        data.advancedSearchArgs.noEpic = 'true';
      } else {
        data.advancedSearchArgs.epicId = this.chosenEpic;
      }
    }
    if (this.chosenVersion !== 'all') {
      if (this.chosenVersion === 'unset') {
        data.advancedSearchArgs.noVersion = 'true';
      } else {
        data.advancedSearchArgs.versionId = this.chosenVersion;
      }
    }
    if (this.onlyMe) {
      data.advancedSearchArgs.ownIssue = 'true';
    }
    if (this.recent) {
      data.advancedSearchArgs.onlyStory = 'true';
    }
    return data;
  }

  axiosGetColorLookupValue() {
    return commonApi.loadLookupValue('epic_color');
  }

  @computed get getColorLookupValue() {
    return this.colorLookupValue;
  }

  @action setColorLookupValue(data) {
    this.colorLookupValue = data;
  }

  @observable randomFeatureColor = {};

  @action setRandomFeatureColor(featureList, colorLookupValue) {
    featureList.forEach((feature) => {
      if (!feature.color) {
        this.randomFeatureColor[feature.summary] = randomItem(colorLookupValue.map((c) => c.name));
      }
    });
  }

  @computed get getSprintWidth() {
    return this.sprintWidth;
  }

  @action setSprintWidth() {
    this.sprintWidth = document.getElementsByClassName('c7n-backlog-sprint')[0].offsetWidth;
  }

  @computed get getOnlyMe() {
    return this.onlyMe;
  }

  @action setOnlyMe(data) {
    this.onlyMe = data;
  }

  @computed get getRecent() {
    return this.recent;
  }

  @action setRecent(data) {
    this.recent = data;
  }

  @action updateSprint(sprintId, newData) {
    const sprint = find(this.sprintData, { sprintId });
    Object.assign(sprint, newData);
  }

  @computed get getClickIssueDetail() {
    return this.clickIssueDetail;
  }

  @computed get getClickIssueId() {
    return toJS(this.clickIssueId);
  }

  @computed get getPrevClickedIssue() {
    return this.prevClickedIssue;
  }

  @computed get getIsLeaveSprint() {
    return this.isLeaveSprint;
  }

  @action setIsLeaveSprint(data) {
    this.isLeaveSprint = data;
  }

  @computed get getIsDragging() {
    return this.isDragging;
  }

  @action setIsDragging(data) {
    this.isDragging = data;
  }

  @computed get getChosenVersion() {
    return this.chosenVersion;
  }

  @action setChosenVersion(data) {
    if (data === 'all') {
      this.filterSelected = false;
    }
    this.spinIf = true;
    this.addToVersionFilter(data);
    this.chosenVersion = data;
  }

  @computed get getChosenEpic() {
    return this.chosenEpic;
  }

  @action setChosenEpic(data) {
    if (data === 'all') {
      this.filterSelected = false;
    }
    this.spinIf = true;
    this.addToEpicFilter(data);
    this.chosenEpic = data;
  }

  @computed get getChosenFeature() {
    return this.chosenFeature;
  }

  @action setChosenFeature(data) {
    if (data === 'all') {
      this.filterSelected = false;
    }
    this.spinIf = true;
    this.addToFeatureFilter(data);
    this.chosenFeature = data;
  }

  @action setEpicData(data) {
    this.epicList = sortBy(data, 'epicRank');
  }

  @action setFeatureData(data) {
    this.featureList = sortBy(data, 'featureRank');
  }

  @action setSelectedPiId(data) {
    this.selectedPiId = data;
  }

  @action setSelectedSprintId(data) {
    this.selectedSprintId = data;
  }

  @action
  async updateSprintInfo() {
    const sprintData = await this.axiosGetSprint();
    this.sprintData.forEach(((s) => {
      const sprint = find(sprintData, (a) => String(a.sprintId) === String(s.sprintId));
      if (sprint) {
        Object.assign(s, pick(sprint, ['issueCount', 'assigneeIssues']));
      }
    }));
  }

  @computed get getExpandSprint() {
    return this.sprintData.filter((s) => s.expand).map((s) => (s.sprintId.toString()));
  }

  @action setSprintData(sprintData) {
    const cached = localPageCacheStore.getItem('backlogSprintExpand');
    const cachedPageSize = localPageCacheStore.getItem('backlogSprintPageSize');
    const previousExpand = new Map(this.sprintData.map((s) => ([s.sprintId.toString(), s.expand])));
    const previousPagination = new Map(this.sprintData.map((s) => ([s.sprintId.toString(), s.pagination])));
    this.sprintData = sprintData.map((sprint, index) => {
      if (!this.issueMap.has(sprint.sprintId.toString())) {
        this.issueMap.set(sprint.sprintId.toString(), []);
      }
      const isBacklog = String(sprint.sprintId) === '0';
      // 这里只保留几个字段，省内存
      return {
        ...sprint,
        expand: cached ? cached.includes(sprint.sprintId.toString()) : !!(previousExpand.get(sprint.sprintId.toString()) ?? index === 0),
        pagination: previousPagination.get(sprint.sprintId.toString()) ?? {
          page: 1,
          size: isObject(cachedPageSize) && cachedPageSize[sprint.sprintId.toString()] ? cachedPageSize[sprint.sprintId.toString()] : 300,
          total: 0,
        },
        // 是否加载过第一次，用了判断展开需不需要加载
        loaded: false,
        loading: false,
        ...isBacklog ? {
          type: 'backlog',
          sprintName: '待办事项',
          sprintId: 0,
        } : {
          type: 'sprint',
          sprintType: sprint.type, // 冲刺类型 用于辨别ip冲刺
        },
      };
    });
    this.initSingleSprint(this.sprintData);
    this.spinIf = false;
  }

  @computed get getSprintPageSize() {
    const data = {};
    this.sprintData.forEach((sprint) => {
      data[sprint.sprintId.toString()] = sprint.pagination.size;
    });
    return data;
  }

  @observable expandedIssueMap = observable.map();

  @action
  toggle(issueId) {
    const isExpand = this.isExpand(issueId);
    // console.log(isExpand);
    this.expandedIssueMap.set(issueId, !isExpand);
  }

  @action
  isExpand(issueId) {
    return this.expandedIssueMap.get(issueId);
  }

  @observable assigneeFilterIds = [];

  @computed get getAssigneeFilterIds() {
    return this.assigneeFilterIds;
  }

  @action setAssigneeFilterIds(data) {
    this.spinIf = true;
    if (data.length > 0) {
      this.filterSelected = true;
    } else if (!this.Judge || (!this.Judge.onlyMeChecked && !this.Judge.moreChecked.length && (!this.Judge.onlyStoryChecked || this.whichVisible === 'feature'))) {
      this.filterSelected = false;
    }
    this.assigneeFilterIds = data;
  }

  // 过滤选中冲刺中的经办人
  @observable filterSprintAssign = observable.map();

  @action('初始化各冲刺下经办人筛选')
  initFilterAssign(sprintData = []) {
    sprintData.forEach((sprint) => {
      const { sprintId, assigneeIssues } = sprint;
      const assigneeId = localPageCacheStore.getItem(`backlog.sprint-${sprintId}`);
      const sprintDefaultAssignee = assigneeId ? find(assigneeIssues, (item) => String(item.assigneeId) === String(assigneeId)) : undefined;
      if (sprintDefaultAssignee) {
        this.filterSprintAssign.set(sprintId, assigneeId);
        this.setFilterSprintAssignUser(sprintId, {
          id: assigneeId,
          imageUrl: sprintDefaultAssignee.imageUrl,
          loginName: sprintDefaultAssignee.assigneeLoginName,
          realName: sprintDefaultAssignee.assigneeRealName,
          tooltip: sprintDefaultAssignee.assigneeName,
        });
      } else {
        localPageCacheStore.remove(`backlog.sprint-${sprintId}`);
      }
      this.filterSprintAssign.set(sprintId, assigneeId);
    });
  }

  @action setFilterSprintAssign(sprintId, assigneeId) {
    this.filterSprintAssign.set(sprintId, assigneeId);
    this.refreshSprint(sprintId);
  }

  @observable filterSprintAssignUser = observable.map();

  @action setFilterSprintAssignUser(sprintId, assignee) {
    this.filterSprintAssignUser.set(sprintId, assignee);
  }

  @action clearFilterSprintAssign(sprintId) {
    this.filterSprintAssign.delete(sprintId);
    this.filterSprintAssignUser.delete(sprintId);
    this.refreshSprint(sprintId);
  }

  axiosGetSprint = () => sprintApi.getBacklogSprintsInfo(this.filter)

  @computed get getIssueTypes() {
    return this.issueTypes;
  }

  @computed get getDefaultPriority() {
    return this.defaultPriority;
  }

  @action setDefaultPriority(data) {
    this.defaultPriority = data;
  }

  @action setSpinIf(data) {
    this.spinIf = data;
  }

  @computed get getSpinIf() {
    return this.spinIf;
  }

  @action initBacklogData(issueTypesData, priorityArrData, sprintData) {
    this.issueCantDrag = false;
    this.onBlurClick();
    if (issueTypesData && !issueTypesData.failed) {
      this.issueTypes = issueTypesData;
    }
    if (priorityArrData && !priorityArrData.failed) {
      this.defaultPriority = priorityArrData;
    }
    this.setSprintData(sprintData);

    this.hasActiveSprint = Boolean(sprintData.find((element) => element.statusCode === 'started'));
    this.spinIf = false;
  }

  @computed get getHasActiveSprint() {
    return this.hasActiveSprint;
  }

  @computed get getSprintData() {
    return this.sprintData;
  }

  @action toggleVisible(data) {
    this.whichVisible = data;
  }

  @computed get getCurrentVisible() {
    return this.whichVisible;
  }

  @observable detailProps = {};

  @action setDetailProps = (data) => {
    this.detailProps = data;
  }

  checkStartAndEnd = (prevIndex, currentIndex) => (prevIndex > currentIndex ? [currentIndex, prevIndex] : [prevIndex, currentIndex]);

  @action dealWithMultiSelect(sprintId, currentClick, type) {
    const data = this.issueMap.get(sprintId);
    const currentIndex = data.findIndex((issue) => currentClick.issueId === issue.issueId);
    if (this.prevClickedIssue && this.prevClickedIssue.sprintId === currentClick.sprintId) {
      // 如果以后想利用 ctrl 从多个冲刺中选取 issue，可以把判断条件2直接挪到 shift 上
      // 但是请考虑清楚操作多个数组可能带来的性能开销问题
      if (type === 'shift') {
        this.dealWithShift(data, currentIndex, sprintId);
      } else if (type === 'ctrl') {
        this.dealWithCtrl(data, currentIndex, currentClick);
      }
    } else {
      this.clickedOnce(sprintId, currentClick, true);
    }
  }

  @action dealWithShift(data, currentIndex, sprintId) {
    const [startIndex, endIndex] = this.checkStartAndEnd(this.prevClickedIssue.index, currentIndex);

    for (let i = startIndex; i <= endIndex; i += 1) {
      // if (this.whichVisible === 'feature' && data[i].issueTypeVO.typeCode === 'story') {
      // this.multiSelected.set(data[i].issueId, data[i]);
      // } else {
      // (issue) => String(issue.assigneeId) === String(filterAssignId)
      // 有过滤，则只选过滤后的问题
      this.multiSelected.set(data[i].issueId, data[i]);
      // }
    }
  }

  @action dealWithCtrl(data, currentIndex, item) {
    // console.log(data, currentIndex, item);
    // if (this.whichVisible === 'feature' && item.issueTypeVO.typeCode !== 'story') {
    //   return;
    // }
    if (this.multiSelected.has(item.issueId)) {
      const prevClickedStatus = this.multiSelected.get(item.issueId);
      if (prevClickedStatus) {
        this.multiSelected.delete(item.issueId);
      } else {
        this.multiSelected.set(item.issueId, item);
      }
    } else {
      this.multiSelected.set(data[currentIndex].issueId, data[currentIndex]);
    }
    this.prevClickedIssue = {
      ...item,
      index: currentIndex,
    };
  }

  @action clickedOnce(sprintId, currentClick, hasExtraKey, isSubIssue) {
    const setData = () => {
      if (!isSubIssue) {
        const index = this.issueMap.get(sprintId.toString()).findIndex((issue) => issue.issueId === currentClick.issueId);
        this.multiSelected = observable.map();
        this.multiSelected.set(currentClick.issueId, currentClick);
        this.prevClickedIssue = {
          ...currentClick,
          index,
        };
      }

      if (!hasExtraKey) {
        this.setClickIssueDetail(currentClick, false);
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

  @action setClickIssueDetail(data, confirm = true) {
    const setData = () => {
      if (!this.multiSelected.get(data.issueId)) {
        this.multiSelected.clear();
        this.multiSelected.set(data.issueId, data);
      }
      this.clickIssueDetail = data;
      if (this.clickIssueDetail) {
        this.clickIssueId = data.issueId;
      }
    };

    if (!confirm || !this.detailProps.descriptionChanged) {
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

  @computed get getMultiSelected() {
    return this.multiSelected;
  }

  @action setCurrentDrag(issueId) {
    this.currentDrag.set(issueId, true);
  }

  @computed get getCurrentDrag() {
    return this.currentDrag;
  }

  @action resetData() {
    this.createdSprint = '';
    this.whichVisible = null;
    this.assigneeFilterIds = [];
    this.multiSelected = observable.map();
    this.sprintData = [];
    this.clickIssueDetail = {};
    this.issueMap.clear();
    this.selectedPiId = undefined;
    this.selectedSprintId = undefined;
  }

  @computed get getIssueMap() {
    return this.issueMap;
  }

  getModifiedArr = (dragItem, type) => {
    const result = [];
    if (!this.multiSelected.has(dragItem.issueId) || type === 'single') {
      result.push(dragItem.issueId);
    }
    if (type === 'multi') {
      result.push(...this.multiSelected.keys());
    }
    return result;
  };

  findOutsetIssue = (sourceIndex, destinationIndex, sourceId, destinationId, destinationArr) => {
    // 看不懂就去让后端给你逐字逐句解释去, 解释不通就怼他
    if (sourceId === destinationId) {
      if (sourceIndex < destinationIndex) {
        return destinationArr[destinationIndex];
      } if (destinationIndex === 0) {
        return destinationArr[destinationIndex];
      }
      return destinationArr[destinationIndex - 1];
    } if (destinationIndex === 0 && destinationArr.length) {
      return destinationArr[destinationIndex];
    }
    return destinationArr[destinationIndex - 1];
  };

  @action moveSingleIssue(destinationId, destinationIndex, sourceId, sourceIndex, draggableId, issueItem, type) {
    const sourceArr = this.issueMap.get(sourceId);
    // const revertSourceArr = sourceArr.slice();
    const destinationArr = this.issueMap.get(destinationId);
    // const revertDestinationArr = destinationArr.slice();
    const prevIssue = this.findOutsetIssue(sourceIndex, destinationIndex, sourceId, destinationId, destinationArr);
    const modifiedArr = this.getModifiedArr(issueItem, type);
    const count = modifiedArr.length;

    if (type === 'single') {
      sourceArr.splice(sourceIndex, 1);
      destinationArr.splice(destinationIndex, 0, issueItem);
      this.issueMap.set(sourceId, sourceArr);
      this.issueMap.set(destinationId, destinationArr);
    } else if (type === 'multi') {
      const modifiedSourceArr = sourceArr.filter((issue) => !this.multiSelected.has(issue.issueId));
      destinationArr.splice(destinationIndex, 0, ...[...this.multiSelected.values()]);
      if (!this.multiSelected.has(issueItem.issueId)) {
        modifiedSourceArr.splice(sourceIndex, 1);
        destinationArr.unshift(issueItem);
      }
      if (sourceId === destinationId) {
        const dragInSingleSprint = sourceArr.filter((issue) => !this.multiSelected.has(issue.issueId));
        dragInSingleSprint.splice(destinationIndex, 0, ...[...this.multiSelected.values()]);
        this.issueMap.set(destinationId, dragInSingleSprint);
      } else {
        this.issueMap.set(sourceId, modifiedSourceArr);
        this.issueMap.set(destinationId, destinationArr);
      }
    }
    // this.multiSelected = observable.map();
    // this.clickIssueDetail = {};
    this.onBlurClick();
    return sprintApi.addIssues(destinationId, {
      before: destinationIndex === 0,
      issueIds: modifiedArr,
      outsetIssueId: prevIssue ? prevIssue.issueId : 0,
      rankIndex: destinationId * 1 === 0 || (destinationId === sourceId && destinationId !== 0),
    }).then((res) => {
      // 先刷新冲刺信息
      this.updateSprintInfo().then(() => {
        // 拖动完刷新
        // 如果是同一个冲刺，当前页刷新就行
        if (sourceId === destinationId) {
          this.refreshSprint(sourceId, false);
        } else {
          // 如果是不同冲刺，那么有两种情况
          const sourceCount = this.issueMap.get(sourceId).length;
          // 如果空了，分页减一
          if (sourceCount === 0) {
            const pagination = this.getPagination(sourceId);
            this.updatePagination(sourceId, {
              // 最小为1
              page: Math.max(pagination.page - 1, 1),
            });
          }
          const destinationPagination = this.getPagination(destinationId);
          this.updatePagination(destinationId, {
            total: destinationPagination.total + count,
          });
          this.refreshSprint(sourceId, false);
        }
        this.spinIf = false;
      }).catch((err) => {
        console.log(err);
        this.refreshSprint(sourceId, false);
        this.refreshSprint(destinationId, false);
      });
    });
  }

  @action setIssueWithEpicOrVersion(item) {
    this.selectedIssueId = this.getModifiedArr(item, this.multiSelected.size > 1 ? 'multi' : 'single');
  }

  @computed get getIssueWithEpicOrVersion() {
    return this.selectedIssueId;
  }

  @action createIssue(issue, sprintId) {
    const issueList = this.issueMap.get(sprintId);
    if (issueList) {
      const modifiedArr = [...issueList, issue];
      this.issueMap.set(sprintId, modifiedArr);
    }
  }

  @action addEpic(data) {
    this.epicList.unshift(data);
  }

  @action initEpicList(epiclist, { lookupValues }) {
    this.colorLookupValue = lookupValues;
    this.epicList = sortBy(epiclist, 'epicRank');
  }

  @computed get getEpicData() {
    return this.epicList;
  }

  @computed get getFeatureData() {
    return this.featureList;
  }

  @action updateEpic(epic) {
    const updateIndex = this.epicList.findIndex((item) => epic.issueId === item.issueId);
    this.epicList[updateIndex].name = epic.name;
    this.epicList[updateIndex].objectVersionNumber = epic.objectVersionNumber;
  }

  @action updateFeature(feature) {
    const updateIndex = this.featureList.findIndex((item) => feature.issueId === item.issueId);
    this.featureList[updateIndex].name = feature.name;
    this.featureList[updateIndex].objectVersionNumber = feature.objectVersionNumber;
  }

  @action moveEpic(sourceIndex, destinationIndex) {
    const movedItem = this.epicList[sourceIndex];
    const { issueId, epicRankObjectVersionNumber } = movedItem;
    this.epicList.splice(sourceIndex, 1);
    this.epicList.splice(destinationIndex, 0, movedItem);
    const before = destinationIndex < this.epicList.length - 1;
    const referenceIssueId = before ? this.epicList[destinationIndex + 1].issueId : this.epicList[destinationIndex - 1].issueId;
    const sortVO = {
      projectId: getProjectId(),
      objectVersionNumber: epicRankObjectVersionNumber, // 乐观锁
      issueId,
      type: 'epic',
      before,
      referenceIssueId,
    };
    storyMapApi.sort(sortVO).then(
      action('fetchSuccess', (res) => {
        if (!res.message) {
          epicApi.loadEpics().then((epics) => {
            this.setEpicData(epics);
          });
        } else {
          this.epicList.splice(destinationIndex, 1);
          this.epicList.splice(sourceIndex, 0, movedItem);
        }
      }),
    );
  }

  @action moveFeature(sourceIndex, destinationIndex) {
    if (sourceIndex === destinationIndex) {
      return;
    }
    const movedItem = this.featureList[sourceIndex];
    const { issueId, featureRankObjectVersionNumber } = movedItem;
    this.featureList.splice(sourceIndex, 1);
    this.featureList.splice(destinationIndex, 0, movedItem);
    const before = destinationIndex < this.featureList.length - 1;
    const referenceIssueId = before ? this.featureList[destinationIndex + 1].issueId : this.featureList[destinationIndex - 1].issueId;
    const sortVO = {
      projectId: getProjectId(),
      objectVersionNumber: featureRankObjectVersionNumber, // 乐观锁
      issueId,
      type: 'feature',
      before,
      referenceIssueId,
    };
    storyMapApi.sort(sortVO).then(
      action('fetchSuccess', (res) => {
        if (!res.message) {
          featureApi.getByPiIdInSubProject(this.selectedPiId, this.selectedSprintId).then((data) => {
            this.setFeatureData(data);
          });
        } else {
          this.featureList.splice(destinationIndex, 1);
          this.featureList.splice(sourceIndex, 0, movedItem);
        }
      }),
    );
  }

  @action addVersion(data) {
    this.versionData.unshift(data);
  }

  @action setVersionData(data) {
    this.versionData = data.sort((a, b) => b.sequence - a.sequence);
  }

  @computed get getVersionData() {
    return this.versionData;
  }

  @action updateVersion(version, type) {
    const updateIndex = this.versionData.findIndex((item) => item.versionId === version.versionId);
    if (type === 'name') {
      this.versionData[updateIndex].name = version.name;
    } else if (type === 'description') {
      this.versionData[updateIndex].description = version.description;
    } else if (type === 'date') {
      this.versionData[updateIndex].startDate = version.startDate;
      this.versionData[updateIndex].expectReleaseDate = version.expectReleaseDate;
    }
    this.versionData[updateIndex].objectVersionNumber = version.objectVersionNumber;
  }

  @action moveVersion(sourceIndex, destinationIndex) {
    const movedItem = this.versionData[sourceIndex];
    const { versionId, objectVersionNumber } = movedItem;
    this.versionData.splice(sourceIndex, 1);
    this.versionData.splice(destinationIndex, 0, movedItem);
    const req = {
      beforeSequence: destinationIndex !== 0 ? this.versionData[destinationIndex - 1].sequence : null,
      afterSequence: destinationIndex !== (this.versionData.length - 1) ? this.versionData[destinationIndex + 1].sequence : null,
      versionId,
      objectVersionNumber,
    };
    versionApi.drag(req).then(
      action('fetchSuccess', (res) => {
        if (!res.message) {
          versionApi.loadAll().then((versions) => {
            this.setVersionData(versions);
          });
        } else {
          this.versionData.splice(destinationIndex, 1);
          this.versionData.splice(sourceIndex, 0, movedItem);
        }
      }),
    );
  }

  @action addToEpicFilter(data) {
    this.filterSelected = true;
    if (data === 'unset') {
      delete this.filter.advancedSearchArgs.epicId;
      this.filter.advancedSearchArgs.noEpic = 'true';
    } else if (data === 'all') {
      delete this.filter.advancedSearchArgs.noEpic;
      delete this.filter.advancedSearchArgs.epicId;
    } else {
      delete this.filter.advancedSearchArgs.noEpic;
      this.filter.advancedSearchArgs.epicId = data;
    }
  }

  @action addToFeatureFilter(data) {
    this.filterSelected = true;
    if (data === 'unset') {
      delete this.filter.advancedSearchArgs.featureId;
      this.filter.advancedSearchArgs.noFeature = 'true';
    } else if (data === 'all') {
      delete this.filter.advancedSearchArgs.noFeature;
      delete this.filter.advancedSearchArgs.featureId;
    } else {
      delete this.filter.advancedSearchArgs.noFeature;
      this.filter.advancedSearchArgs.featureId = data;
    }
  }

  @action addToVersionFilter(data) {
    this.filterSelected = true;
    if (data === 'unset') {
      delete this.filter.advancedSearchArgs.versionId;
      this.filter.advancedSearchArgs.noVersion = 'true';
    } else if (data === 'all') {
      this.filterSelected = false;
      delete this.filter.advancedSearchArgs.noVersion;
      delete this.filter.advancedSearchArgs.versionId;
    } else {
      delete this.filter.advancedSearchArgs.noVersion;
      this.filter.advancedSearchArgs.versionId = data;
    }
  }

  @action resetFilter() {
    this.spinIf = true;
    if (this.whichVisible === 'feature') {
      this.filter = { advancedSearchArgs: { onlyStory: 'true' } };
    } else {
      this.filter = { advancedSearchArgs: {} };
    }
    this.versionFilter = 'all';
    this.epicFilter = 'all';
    this.quickFilters = [];
    this.assigneeFilterIds = [];
    this.filterSprintAssign = observable.map();
    this.chosenEpic = 'all';
    this.chosenVersion = 'all';
    this.filterSelected = false;
    this.chosenFeature = 'all';
  }

  @action setFilterSelected(filterSelected) {
    this.filterSelected = filterSelected;
  }

  @computed get hasFilter() {
    return this.filterSelected;
  }

  @action clearSprintFilter() {
    this.resetFilter();
    this.axiosGetSprint().then(action('fetchSuccess', (res) => {
      this.setSprintData(res);
      this.spinIf = false;
    }));
  }

  @action setQuickFilters(onlyMeChecked, onlyStoryChecked, moreChecked = []) {
    this.spinIf = true;
    this.Judge = {
      onlyMeChecked, onlyStoryChecked, moreChecked,
    };
    if (onlyMeChecked) {
      this.filter.advancedSearchArgs.ownIssue = 'true';
      this.filterSelected = true;
    } else {
      delete this.filter.advancedSearchArgs.ownIssue;
    }
    if (onlyStoryChecked) {
      this.filter.advancedSearchArgs.onlyStory = 'true';
      if (this.whichVisible !== 'feature') {
        this.filterSelected = true;
      }
    } else {
      delete this.filter.advancedSearchArgs.onlyStory;
    }
    this.quickFilters = moreChecked;
    if (moreChecked.length) {
      this.filterSelected = true;
    }

    // 如果一个都没选，则不显示清空
    if (!onlyMeChecked && !onlyStoryChecked && !moreChecked.length && !this.assigneeFilterIds.length === 0) {
      this.filterSelected = false;
    }
  }

  @action toggleIssueDrag(data) {
    this.issueCantDrag = data;
  }

  @computed get getIssueCantDrag() {
    return this.issueCantDrag;
  }

  @action onBlurClick() {
    this.multiSelected = observable.map();
    if (this.clickIssueDetail && this.clickIssueDetail.issueId) {
      this.multiSelected.set(this.clickIssueDetail.issueId, this.clickIssueDetail);
    }
  }

  @action clearMultiSelected() {
    this.multiSelected = observable.map();
  }

  @action setCreatedSprint(data) {
    this.createdSprint = data;
  }

  @action expandSprint(sprintId, expand) {
    const sprint = find(this.sprintData, { sprintId });
    sprint.expand = expand;
    localPageCacheStore.setItem('backlogSprintExpand', this.getExpandSprint);
    if (expand && !sprint.loaded) {
      this.refreshSprint(sprintId);
    }
  }

  /**
   * 加载选择快速搜索的冲刺数据
   */
  getSprint = async (setPiIdIf, setAssigner = false) => {
    const [issueTypes, priorityArr, sprintData] = await Promise.all([
      issueTypeApi.loadAllWithStateMachineId(),
      priorityApi.getDefaultByProject(),
      this.axiosGetSprint(),
    ]);
    setAssigner && this.initFilterAssign(sprintData);
    await this.getPlanPi(sprintData, setPiIdIf);
    this.initBacklogData(issueTypes, priorityArr, sprintData);
  };

  getPlanPi = async (sprintData = this.sprintData, setPiIdIf = true) => {
    if (isInProgram()) {
      const program = await commonApi.getProjectsInProgram();
      if (!program) {
        return;
      }
      const notDonePiList = await piApi.getPiByPiStatus(['todo', 'doing'], program?.id);
      // 为了可以对规划中的冲刺进行时间修改的限制，这里获取对应pi和冲刺
      const piIds = intersection(notDonePiList.map((pi) => pi.id), uniq(sprintData.filter((sprint) => sprint.planning).map((sprint) => sprint.piId)));
      if (piIds.length > 0) {
        const sprints = await Promise.all(piIds.map((piId) => sprintApi.getAllByPiId(piId)));
        this.setPlanPiAndSprints(piIds, notDonePiList, sprints);
      }
      this.notDonePiList = notDonePiList;
      const doingPi = notDonePiList.find((pi) => pi.statusCode === 'doing');
      if (doingPi && setPiIdIf) {
        this.setSelectedPiId(doingPi.id);
      }
    }
  }

  /**
   * 加载版本数据
   */
  loadVersion = () => {
    versionApi.loadAll().then((data2) => {
      const newVersion = [...data2];
      for (let index = 0, len = newVersion.length; index < len; index += 1) {
        newVersion[index].expand = false;
      }
      this.setVersionData(newVersion);
    }).catch((error) => {
    });
  };

  /**
   * 加载史诗
   */
  loadEpic = () => {
    epicApi.loadEpics().then((data3) => {
      const newEpic = [...data3];
      for (let index = 0, len = newEpic.length; index < len; index += 1) {
        newEpic[index].expand = false;
      }
      this.setEpicData(newEpic);
    }).catch((error3) => {
    });
  };

  /**
   * 加载特性
   */
  loadFeature = () => {
    featureApi.getByPiIdInSubProject(this.selectedPiId, this.selectedSprintId).then((data) => {
      this.setFeatureData(data);
    }).catch(() => {
    });
  };

  /**
   * @param setAssigner 设置经办人
   */
  refresh = (spinIf = true, setPiIdIf = true, setAssigner = true) => {
    // if (this.IssueDetail) {
    //   this.IssueDetail.refreshIssueDetail();
    // }
    if (spinIf) {
      this.setSpinIf(true);
    }
    this.getSprint(setPiIdIf, setAssigner);
    if (this.getCurrentVisible === 'version') {
      this.loadVersion();
    } else if (this.getCurrentVisible === 'epic') {
      this.loadEpic();
    } else if (this.getCurrentVisible === 'feature') {
      this.loadFeature();
    }
  };

  handleDeleteSprint = async (data, isCurrentPi) => {
    // const defaultValueIsCurrent = await sprintApi.beforeChangeCheck(data.sprintId);
    const defaultValuePrompt = undefined;// defaultValueIsCurrent ? `冲刺${data.sprintName}是默认选项，删除后冲刺字段默认值将清空` : undefined;
    const hasIssue = data.issueSearchVOList && data.issueSearchVOList.length > 0;
    Modal.open({
      title: '删除冲刺',
      children: hasIssue ? (
        <div>
          <p style={{ marginBottom: 10 }}>{`确认要删除冲刺“${data.sprintName}”吗？删除冲刺后当前规划的问题将移动到待办事项。`}</p>
          {defaultValuePrompt && <p style={{ marginBottom: 10 }}>{defaultValuePrompt}</p>}
        </div>
      ) : (
        <div>
          <p style={{ marginBottom: 10 }}>{`确认要删除冲刺“${data.sprintName}”吗？`}</p>
          {defaultValuePrompt && <p style={{ marginBottom: 10 }}>{defaultValuePrompt}</p>}
        </div>
      ),
      onOk: async () => {
        await sprintApi.delete(data.sprintId, isCurrentPi);
        this.refresh();
      },
      okText: '删除',
    });
  };

  handleCreateIssue(res, sprintId) {
    this.createIssue({
      ...res,
      imageUrl: res.assigneeImageUrl,
      versionIds: res.versionIssueRelVOList.length ? [res.versionIssueRelVOList[0].versionId] : [],
      versionNames: res.versionIssueRelVOList.length ? [res.versionIssueRelVOList[0].name] : [],
    }, sprintId);
  }

  handleIssueClick(e, item, sprintId, isSubIssue) {
    e.stopPropagation();
    if (!(e.shiftKey && (e.ctrlKey || e.metaKey))) {
      if (e.shiftKey) {
        if (!isSubIssue) {
          this.dealWithMultiSelect(sprintId, item, 'shift');
        }
      } else if (e.ctrlKey || e.metaKey) {
        if (!isSubIssue) {
          this.dealWithMultiSelect(sprintId, item, 'ctrl');
        }
      } else {
        this.clickedOnce(sprintId, item, e.shiftKey || e.ctrlKey || e.metaKey, isSubIssue);
      }
    }
  }

  onDragEnd = (result) => {
    this.setIsDragging(null);
    const { destination, source, draggableId } = result;
    if (destination) {
      const { droppableId: destinationId, index: destinationIndex } = destination;
      const { droppableId: sourceId, index: sourceIndex } = source;
      if (destinationId === sourceId && destinationIndex === sourceIndex) {
        return;
      }
      if (result.reason !== 'CANCEL') {
        const item = this.getIssueMap.get(sourceId)[sourceIndex];
        const destinationArr = this.getIssueMap.get(destinationId);
        let destinationItem;
        if (destinationIndex === 0) {
          destinationItem = null;
        } else if (destinationIndex === this.getIssueMap.get(destinationId).length) {
          destinationItem = destinationArr[destinationIndex - 1];
        } else {
          destinationItem = destinationArr[destinationIndex];
        }
        if (this.getMultiSelected.size > 1 && !this.getMultiSelected.has(destinationItem)) {
          this.moveSingleIssue(destinationId, destinationIndex, sourceId, sourceIndex, draggableId, item, 'multi').then(() => {
            if (this.IssueDetail) {
              this.IssueDetail.refreshIssueDetail();
            }
          });
        } else {
          this.moveSingleIssue(destinationId, destinationIndex, sourceId, sourceIndex, draggableId, item, 'single').then(() => {
            if (this.IssueDetail) {
              this.IssueDetail.refreshIssueDetail();
            }
          });
        }
      }
    }
  }

  onBeforeCapture = ({ draggableId }) => {
    this.setIsDragging(draggableId);
  }

  onDragStart = (result) => {
    // console.log('onDragStart', result);
    const { source, draggableId } = result;
    const { droppableId: sourceId, index: sourceIndex } = source;
    const item = this.getIssueMap.get(sourceId)[sourceIndex];
    // this.setIsDragging(item.issueId);
    this.setIssueWithEpicOrVersion(item);
  }

  @observable startSprintVisible = false;

  @observable closeSprintVisible = false;

  @action setStartSprintVisible(startSprintVisible) {
    this.startSprintVisible = startSprintVisible;
  }

  @action setCloseSprintVisible(closeSprintVisible) {
    this.closeSprintVisible = closeSprintVisible;
  }

  getIssueListBySprintId(sprintId) {
    const issueList = this.issueMap.get(String(sprintId));
    return issueList;
  }

  @observable showPlanSprint = true;

  @action setShowPlanSprint(showPlanSprint) {
    this.showPlanSprint = showPlanSprint;
  }

  @observable notDonePiList = []

  piMap = observable.map()

  @action setPlanPiAndSprints(piIds, pis, sprints) {
    piIds.forEach((piId, index) => {
      const pi = find(pis, { id: piId });
      this.piMap.set(piId, {
        pi,
        sprints: sprints[index],
      });
    });
  }

  @observable piInfo = {};

  @observable sprints = []; // 用于时间判断

  loadPiInfoAndSprint = async () => {
    const artInfo = await commonApi.getIsShowFeature();
    const currentPiInfo = await piApi.getCurrent(artInfo?.programId, artInfo?.id);
    if (currentPiInfo.id) {
      const sprints = await sprintApi.getAllByPiId(currentPiInfo.id);
      this.setPiInfo(currentPiInfo);
      this.setSprints(sprints.map((sprint) => ({
        ...sprint,
        endDate: sprint.actualEndDate || sprint.endDate,
      })));
    }
    return [];
  }

  @action setPiInfo(data) {
    this.piInfo = data;
  }

  @computed get getPiInfo() {
    return this.piInfo;
  }

  @action setSprints(data) {
    this.sprints = data;
  }

  @computed get getSprints() {
    return this.sprints.slice();
  }

  // 时间点是否在pi内
  isDateBetweenPiDate({ date, pi = this.getPiInfo } = {}) {
    const { endDate: piEndDate } = pi;
    const piActualStartDate = pi.actualStartDate || pi.startDate;
    return date.isBetween(piActualStartDate, piEndDate);
  }

  // 时间点是否在其他冲刺中
  isDateBetweenOtherSprints({ date, sprintId, sprints = this.sprints } = {}) {
    return sprints.filter((sprint) => sprint.sprintId !== sprintId).some((sprint) => {
      const { startDate } = sprint;
      const endDate = sprint.actualEndDate || sprint.endDate;
      return date.isBetween(startDate, endDate);
    });
  }

  // 时间段是否在pi中
  isRangeInPi({ startDate, endDate, pi = this.getPiInfo }) {
    const { endDate: piEndDate } = pi;
    const piActualStartDate = pi.actualStartDate || pi.startDate;
    const piRange = moment.range(piActualStartDate, piEndDate);
    // 开始时间和结束时间都在pi内
    return piRange.contains(startDate) && piRange.contains(endDate);
  }

  // 时间段是否和其他冲刺有重叠
  isRangeOverlapWithOtherSprints({
    startDate, endDate, sprintId, sprints = this.sprints,
  } = {}) {
    return sprints.filter((sprint) => sprint.sprintId !== sprintId).some((sprint) => {
      const { startDate: sprintStartDate } = sprint;
      const sprintEndDate = sprint.actualEndDate || sprint.endDate;
      const sprintRange = moment.range(sprintStartDate, sprintEndDate);
      const range = moment.range(startDate, endDate);
      return range.overlaps(sprintRange);
    });
  }

  // 开始时间应小于结束时间
  isRange(startDate, endDate) {
    return startDate < endDate;
  }

  // 时间能不能选
  dateCanChoose({
    date, sprintId, pi, sprints,
  }) {
    // 首先时间应该在PI的实际开始时间和结束时间之间
    // 并且不能在其他冲刺之间
    return this.isDateBetweenPiDate({ date, pi }) && !this.isDateBetweenOtherSprints({ date, sprintId, sprints });
  }

  // 时间段是不是可以选
  rangeCanChoose({
    startDate, endDate, sprintId, pi, sprints,
  }) {
    // 时间段要在pi内部
    // 时间段不能和其他冲刺重叠
    return this.isRange(startDate, endDate) && this.isRangeInPi({ startDate, endDate, pi }) && !this.isRangeOverlapWithOtherSprints({
      startDate, endDate, sprintId, sprints,
    });
  }

  @observable defaultTypeId = '';

  @action setDefaultTypeId = (data) => {
    this.defaultTypeId = data;
  }

  @observable defaultSummary = '';

  @action setDefaultSummary = (data) => {
    this.defaultSummary = data;
  }

  @observable defaultSprint = '';

  @action setDefaultSprint = (data) => {
    this.defaultSprint = data;
  }

  @observable defaultAssignee;

  @action setDefaultAssignee = (data) => {
    this.defaultAssignee = data;
  }

  @observable defaultEpicName;

  @action setDefaultEpicName = (data) => {
    this.defaultEpicName = data;
  }

  initSingleSprint(sprintData) {
    sprintData.forEach((sprint) => {
      if (sprint.expand) {
        this.refreshSprint(sprint.sprintId);
      }
    });
  }

  @action
  async refreshSprint(sprintId, resetPage = true) {
    const pagination = this.getPagination(sprintId);
    const sprint = this.getTargetSprint(sprintId);
    sprint.loading = true;
    const { list: issueSearchVOList, number, total } = await sprintApi.getIssuesBySprintId(sprintId, {
      advancedSearchArgs: {
        ...this.filter.advancedSearchArgs,
        assigneeFilterIds: [this.filterSprintAssign.get(sprintId)].filter(Boolean),
      },
    },
    resetPage ? 1 : pagination.page,
    pagination.size);
    this.updatePagination(sprintId, {
      total,
      page: number + 1,
    });
    this.issueMap.set(sprintId.toString(), issueSearchVOList);
    sprint.loading = false;
    sprint.loaded = true;
  }

  getTargetSprint(sprintId) {
    const sprint = find(this.sprintData, { sprintId });
    return sprint;
  }

  @action updatePagination(sprintId, pagination) {
    const sprint = find(this.sprintData, { sprintId });
    if (sprint) {
      Object.assign(sprint.pagination, pagination);
    }
  }

  @action getPagination(sprintId, pagination) {
    const sprint = find(this.sprintData, { sprintId });
    return sprint.pagination;
  }
}

const backlogStore = new BacklogStore();
export default backlogStore;
