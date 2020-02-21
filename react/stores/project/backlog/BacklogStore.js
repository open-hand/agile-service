import React from 'react';
import axios from 'axios';
import {
  observable, action, computed, toJS, reaction, set,
} from 'mobx';
import { sortBy, find } from 'lodash';
import { store, stores } from '@choerodon/boot';
import { Modal } from 'choerodon-ui';
import { getFeaturesInProject } from '@/api/FeatureApi';
import { sort } from '@/api/StoryMapApi';
import { getProjectId } from '@/common/utils';

const { AppState } = stores;

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

  @observable quickSearchList = [];

  @observable selectIssues = [];

  @observable issueTypes = [];

  @observable defaultPriority = false;

  @observable cleanQuickSearch = false;

  @observable newIssueVisible = false;


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

  axiosGetProjectInfo() {
    return axios.get(`/agile/v1/projects/${AppState.currentMenuType.id}/project_info`);
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

  axiosDeleteSprint(id) {
    return axios.delete(`/agile/v1/projects/${AppState.currentMenuType.id}/sprint/${id}`);
  }

  axiosGetColorLookupValue() {
    return axios.get('/agile/v1/lookup_values/epic_color');
  }

  @computed get getColorLookupValue() {
    return this.colorLookupValue;
  }

  @action setColorLookupValue(data) {
    this.colorLookupValue = data;
  }

  @computed get getSprintWidth() {
    return this.sprintWidth;
  }

  @action setSprintWidth() {
    this.sprintWidth = document.getElementsByClassName('c7n-backlog-sprint')[0].offsetWidth;
  }

  axiosGetIssueDetail(issueId) {
    const orgId = AppState.currentMenuType.organizationId;
    return axios.get(`/agile/v1/projects/${AppState.currentMenuType.id}/issues/${issueId}${orgId ? `?organizationId=${orgId}` : ''}`);
  }


  axiosGetOpenSprintDetail(sprintId) {
    return axios.get(`/agile/v1/projects/${AppState.currentMenuType.id}/sprint/${sprintId}`);
  }

  axiosStartSprint(data) {
    return axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/sprint/start`, data);
  }

  axiosCloseSprint(data) {
    return axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/sprint/complete`, data);
  }

  axiosGetSprintCompleteMessage(sprintId) {
    return axios.get(`/agile/v1/projects/${AppState.currentMenuType.id}/sprint/${sprintId}/names`);
  }

  axiosEasyCreateIssue(data) {
    return axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/issues?applyType=agile`, data);
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

  axiosUpdateSprint(data) {
    return axios.put(`/agile/v1/projects/${AppState.currentMenuType.id}/sprint`, data);
  }

  @action updateSprint(sprintId, newData) {
    const sprint = find(this.sprintData, { sprintId });
    Object.assign(sprint, newData);
  }

  axiosUpdateVerison(versionId, data) {
    return axios.put(`/agile/v1/projects/${AppState.currentMenuType.id}/product_version/update/${versionId}`, data);
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

  axiosUpdateIssuesToVersion(versionId, ids) {
    return axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/issues/to_version/${versionId}`, ids);
  }

  axiosUpdateIssuesToEpic(epicId, ids) {
    return axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/issues/to_epic/${epicId}`, ids);
  }

  axiosUpdateIssuesToFeature(featureId, ids) {
    return axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/issues/to_feature/${featureId}`, ids);
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

  axiosCreateSprint(data) {
    return axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/sprint`, data);
  }

  axiosUpdateIssuesToSprint(sprintId, data) {
    return axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/issues/to_sprint/${sprintId}`, data);
  }

  axiosUpdateIssue(data) {
    return axios.put(`/agile/v1/projects/${AppState.currentMenuType.id}/issues`, data);
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

  axiosGetEpic() {
    return axios.get(`/agile/v1/projects/${AppState.currentMenuType.id}/issues/epics`);
  }

  axiosGetVersion() {
    return axios.get(`/agile/v1/projects/${AppState.currentMenuType.id}/product_version`);
  }

  @action setSprintData({ backlogData, sprintData }) {    
    this.issueMap.set('0', backlogData.backLogIssue ? backlogData.backLogIssue : []);
    const { backLogIssue, backlogIssueCount } = backlogData;
    this.sprintData = sprintData.map((sprint) => {
      const { issueSearchVOList } = sprint;
      this.issueMap.set(sprint.sprintId.toString(), issueSearchVOList);
      // 这里只保留几个字段，省内存
      return {
        ...sprint,
        issueSearchVOList: null,        
        type: 'sprint',
        expand: true,    
      };
    }).concat({
      type: 'backlog',
      sprintId: 0,
      sprintName: '待办事项',
      expand: true,
      issueCount: backlogIssueCount,
      issueSearchVOList: backLogIssue,
    });
    this.spinIf = false;
  }

  @computed get getQuickSearchList() {
    return toJS(this.quickSearchList);
  }

  @action setQuickSearchList(data) {
    this.quickSearchList = data;
  }

  axiosGetQuickSearchList() {
    return axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/quick_filter/query_all`, {
      contents: [],
      filterName: '',
    });
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

  @action setFilterSprintAssign(sprintId, assigneeId) {
    this.filterSprintAssign.set(sprintId, assigneeId);
  }

  @action clearFilterSprintAssign(sprintId) {
    this.filterSprintAssign.delete(sprintId);
  }

  axiosGetSprint = () => {
    const orgId = AppState.currentMenuType.organizationId;
    return axios.post(`/agile/v1/projects/${AppState.currentMenuType.id}/sprint/issues?organizationId=${orgId}&quickFilterIds=${this.quickFilters}${this.assigneeFilterIds.length > 0 ? `&assigneeFilterIds=${this.assigneeFilterIds}` : ''}`, this.filter);
  }


  handleVersionDrap = data => axios.put(`/agile/v1/projects/${AppState.currentMenuType.id}/product_version/drag`, data);

  axiosGetWorkSetting(year) {
    const proId = AppState.currentMenuType.id;
    const orgId = AppState.currentMenuType.organizationId;
    return axios.get(`/base/v1/projects/${proId}/time_zone_work_calendars/time_zone_detail/${orgId}?year=${year}`);
  }

  @computed get getIssueTypes() {
    return this.issueTypes;
  }

  axiosGetIssueTypes() {
    const proId = AppState.currentMenuType.id;
    return axios.get(`/agile/v1/projects/${proId}/schemes/query_issue_types_with_sm_id?apply_type=agile`);
  }

  @computed get getDefaultPriority() {
    return this.defaultPriority;
  }

  @action setDefaultPriority(data) {
    this.defaultPriority = data;
  }

  axiosGetDefaultPriority() {
    const proId = AppState.currentMenuType.id;
    return axios.get(`/agile/v1/projects/${proId}/priority/default`);
  }

  @action setSpinIf(data) {
    this.spinIf = data;
  }

  @computed get getSpinIf() {
    return this.spinIf;
  }

  @action initBacklogData(quickSearchData, issueTypesData, priorityArrData, { backlogData, sprintData }) {
    this.issueCantDrag = false;
    this.onBlurClick();
    // this.multiSelected = observable.map();
    this.quickSearchList = quickSearchData;
    if (issueTypesData && !issueTypesData.failed) {
      this.issueTypes = issueTypesData;
    }
    if (priorityArrData && !priorityArrData.failed) {
      this.defaultPriority = priorityArrData;
    }
    this.setSprintData({ backlogData, sprintData });

    this.hasActiveSprint = Boolean(sprintData.find(element => element.statusCode === 'started'));
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

  checkStartAndEnd = (prevIndex, currentIndex) => (prevIndex > currentIndex ? [currentIndex, prevIndex] : [prevIndex, currentIndex]);

  @action dealWithMultiSelect(sprintId, currentClick, type) {
    const data = this.issueMap.get(sprintId);
    const currentIndex = data.findIndex(issue => currentClick.issueId === issue.issueId);
    if (this.prevClickedIssue && this.prevClickedIssue.sprintId === currentClick.sprintId) {
      // 如果以后想利用 ctrl 从多个冲刺中选取 issue，可以把判断条件2直接挪到 shift 上
      // 但是请考虑清楚操作多个数组可能带来的性能开销问题
      if (type === 'shift') {
        this.dealWithShift(data, currentIndex);
      } else if (type === 'ctrl') {
        this.dealWithCtrl(data, currentIndex, currentClick);
      }
    } else {
      this.clickedOnce(sprintId, currentClick, true);
    }
  }

  @action dealWithShift(data, currentIndex) {
    const [startIndex, endIndex] = this.checkStartAndEnd(this.prevClickedIssue.index, currentIndex);
    for (let i = startIndex; i <= endIndex; i += 1) {
      // if (this.whichVisible === 'feature' && data[i].issueTypeVO.typeCode === 'story') {
      // this.multiSelected.set(data[i].issueId, data[i]);
      // } else {
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

  @action clickedOnce(sprintId, currentClick, hasExtraKey) {
    const index = this.issueMap.get(sprintId).findIndex(issue => issue.issueId === currentClick.issueId);
    this.multiSelected = observable.map();
    this.multiSelected.set(currentClick.issueId, currentClick);
    this.prevClickedIssue = {
      ...currentClick,
      index,
    };
    if (!hasExtraKey) {
      this.setClickIssueDetail(currentClick);
    }
  }

  @action setClickIssueDetail(data) {
    this.clickIssueDetail = data;
    if (this.clickIssueDetail) {
      this.clickIssueId = data.issueId;
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
  }

  @computed get getIssueMap() {
    const that = this;
    return {
      get(sprintId) {
        const filterAssignId = that.filterSprintAssign.get(Number(sprintId));
        if (filterAssignId) {
          return that.issueMap.get(sprintId) ? that.issueMap.get(sprintId).filter(issue => issue.assigneeId === filterAssignId) : [];
        } else {
          return that.issueMap.get(sprintId);
        }
      },
    };
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
      } else if (destinationIndex === 0) {
        return destinationArr[destinationIndex];
      } else {
        return destinationArr[destinationIndex - 1];
      }
    } else if (destinationIndex === 0 && destinationArr.length) {
      return destinationArr[destinationIndex];
    } else {
      return destinationArr[destinationIndex - 1];
    }
  };

  @action moveSingleIssue(destinationId, destinationIndex, sourceId, sourceIndex, draggableId, issueItem, type) {
    const sourceArr = this.issueMap.get(sourceId);
    // const revertSourceArr = sourceArr.slice();
    const destinationArr = this.issueMap.get(destinationId);
    // const revertDestinationArr = destinationArr.slice();
    const prevIssue = this.findOutsetIssue(sourceIndex, destinationIndex, sourceId, destinationId, destinationArr);
    const modifiedArr = this.getModifiedArr(issueItem, type);

    if (type === 'single') {
      sourceArr.splice(sourceIndex, 1);
      destinationArr.splice(destinationIndex, 0, issueItem);
      this.issueMap.set(sourceId, sourceArr);
      this.issueMap.set(destinationId, destinationArr);
    } else if (type === 'multi') {
      const modifiedSourceArr = sourceArr.filter(issue => !this.multiSelected.has(issue.issueId));
      destinationArr.splice(destinationIndex, 0, ...[...this.multiSelected.values()]);
      if (!this.multiSelected.has(issueItem.issueId)) {
        modifiedSourceArr.splice(sourceIndex, 1);
        destinationArr.unshift(issueItem);
      }
      if (sourceId === destinationId) {
        const dragInSingleSprint = sourceArr.filter(issue => !this.multiSelected.has(issue.issueId));
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
    return axios.post(`agile/v1/projects/${AppState.currentMenuType.id}/issues/to_sprint/${destinationId}`, {
      before: destinationIndex === 0,
      issueIds: modifiedArr,
      outsetIssueId: prevIssue ? prevIssue.issueId : 0,
      rankIndex: destinationId * 1 === 0 || (destinationId === sourceId && destinationId !== 0),
    }).then(this.axiosGetSprint).then((res) => {
      this.setSprintData(res);
      this.spinIf = false;
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
    const updateIndex = this.epicList.findIndex(item => epic.issueId === item.issueId);
    this.epicList[updateIndex].name = epic.name;
    this.epicList[updateIndex].objectVersionNumber = epic.objectVersionNumber;
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
    sort(sortVO).then(
      action('fetchSuccess', (res) => {
        if (!res.message) {
          this.axiosGetEpic().then((epics) => {
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
    sort(sortVO).then(
      action('fetchSuccess', (res) => {
        if (!res.message) {
          getFeaturesInProject().then((data) => {
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
    const updateIndex = this.versionData.findIndex(item => item.versionId === version.versionId);
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
    this.handleVersionDrap(req).then(
      action('fetchSuccess', (res) => {
        if (!res.message) {
          this.axiosGetVersion().then((versions) => {
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
    } else if (typeof data === 'number') {
      delete this.filter.advancedSearchArgs.noEpic;
      this.filter.advancedSearchArgs.epicId = data;
    } else {
      delete this.filter.advancedSearchArgs.noEpic;
      delete this.filter.advancedSearchArgs.epicId;
    }
  }

  @action addToFeatureFilter(data) {
    this.filterSelected = true;
    if (data === 'unset') {
      delete this.filter.advancedSearchArgs.featureId;
      this.filter.advancedSearchArgs.noFeature = 'true';
    } else if (typeof data === 'number') {
      delete this.filter.advancedSearchArgs.noFeature;
      this.filter.advancedSearchArgs.featureId = data;
    } else {
      delete this.filter.advancedSearchArgs.noFeature;
      delete this.filter.advancedSearchArgs.featureId;
    }
  }

  @action addToVersionFilter(data) {
    this.filterSelected = true;
    if (data === 'unset') {
      delete this.filter.advancedSearchArgs.versionId;
      this.filter.advancedSearchArgs.noVersion = 'true';
    } else if (typeof data === 'number') {
      delete this.filter.advancedSearchArgs.noVersion;
      this.filter.advancedSearchArgs.versionId = data;
    } else {
      this.filterSelected = false;
      delete this.filter.advancedSearchArgs.noVersion;
      delete this.filter.advancedSearchArgs.versionId;
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
  }

  /**
   * 加载选择快速搜索的冲刺数据
   */
  getSprint = () => {
    this.axiosGetIssueTypes();
    this.axiosGetDefaultPriority();
    Promise.all([this.axiosGetQuickSearchList(), this.axiosGetIssueTypes(), this.axiosGetDefaultPriority(), this.axiosGetSprint()]).then(([quickSearch, issueTypes, priorityArr, backlogData]) => {
      this.initBacklogData(quickSearch, issueTypes, priorityArr, backlogData);
    });
  };

  /**
   * 加载版本数据
   */
  loadVersion = () => {
    this.axiosGetVersion().then((data2) => {
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
    this.axiosGetEpic().then((data3) => {
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
    getFeaturesInProject().then((data) => {
      this.setFeatureData(data);
    }).catch(() => {
    });
  };

  refresh = (spinIf = true) => {
    // if (this.IssueDetail) {
    //   this.IssueDetail.refreshIssueDetail();
    // }
    if (spinIf) {
      this.setSpinIf(true);
    }
    this.getSprint();
    if (this.getCurrentVisible === 'version') {
      this.loadVersion();
    } else if (this.getCurrentVisible === 'epic') {
      this.loadEpic();
    } else if (this.getCurrentVisible === 'feature') {
      this.loadFeature();
    }
  };

  handleDeleteSprint = (data) => {
    if (data.issueSearchVOList && data.issueSearchVOList.length > 0) {
      Modal.confirm({
        width: 560,
        wrapClassName: 'deleteConfirm',
        title: `删除冲刺${data.sprintName}`,
        content: (
          <div>
            <p style={{ marginBottom: 10 }}>请确认您要删除这个冲刺。</p>
            <p style={{ marginBottom: 10 }}>这个冲刺将会被彻底删除，冲刺中的任务将会被移动到待办事项中。</p>
          </div>
        ),
        onOk() {
          return this.axiosDeleteSprint(data.sprintId).then((res) => {
            this.refresh();
          }).catch((error) => {
          });
        },
        onCancel() { },
        okText: '删除',
        okType: 'danger',
      });
    } else {
      this.axiosDeleteSprint(data.sprintId).then((res) => {
        this.refresh();
      }).catch((error) => {
      });
    }
  };

  handleCreateIssue(res, sprintId) {
    this.createIssue({
      ...res,
      imageUrl: res.assigneeImageUrl,
      versionIds: res.versionIssueRelVOList.length ? [res.versionIssueRelVOList[0].versionId] : [],
      versionNames: res.versionIssueRelVOList.length ? [res.versionIssueRelVOList[0].name] : [],
    }, sprintId);
  }

  handleIssueClick(e, item, sprintId) {
    e.stopPropagation();
    if (!(e.shiftKey && (e.ctrlKey || e.metaKey))) {
      if (e.shiftKey) {
        this.dealWithMultiSelect(sprintId, item, 'shift');
      } else if (e.ctrlKey || e.metaKey) {
        this.dealWithMultiSelect(sprintId, item, 'ctrl');
      } else {
        this.clickedOnce(sprintId, item, e.shiftKey || e.ctrlKey || e.metaKey);
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

  onDragStart = (result) => {
    // console.log('onDragStart', result);
    const { source, draggableId } = result;
    const { droppableId: sourceId, index: sourceIndex } = source;
    const item = this.getIssueMap.get(sourceId)[sourceIndex];
    this.setIsDragging(item.issueId);
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
    const filterAssignId = this.filterSprintAssign.get(sprintId);
    return filterAssignId ? issueList.filter(issue => issue.assigneeId === filterAssignId) : issueList;
  }

  @observable showPlanSprint = true;

  @action setShowPlanSprint(showPlanSprint) {
    this.showPlanSprint = showPlanSprint;
  }
}

const backlogStore = new BacklogStore();
export default backlogStore;
