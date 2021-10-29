import {
  observable, action, computed, set, toJS,
} from 'mobx';
import {
  cloneDeep,
  find, findIndex, remove, sortBy, pick,
} from 'lodash';
import { getProjectId } from '@/utils/common';
import {
  storyMapApi, versionApi, issueTypeApi, priorityApi, sprintApi,
} from '@/api';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import openDescriptionConfirm from '@/components/detail-container/openDescriptionConfirm';

class StoryMapStore {
  @observable swimLine = localStorage.getItem('agile.StoryMap.SwimLine') || 'none';

  @observable sideIssueListVisible = false;

  @observable createModalVisible = false;

  @observable createEpicModalVisible = false;

  @observable createFeatureModalVisible = false;

  @observable isFullScreen = false;

  @observable sideSearchVO = {
    searchArgs: {
      assigneeId: null,
    },
    advancedSearchArgs: {
      versionList: [],
      statusList: [],
    },
  };

  @observable searchVO = {
    advancedSearchArgs: {},
    otherArgs: {
      customField: {
        option: [], date: [], date_hms: [], number: [], string: [], text: [],
      },
    },
    searchArgs: {},
  }

  @action setSearchVO = (data) => {
    this.searchVO = data;
  }

  @observable issueList = [];

  @observable issueTypes = [];

  @observable prioritys = [];

  @observable versionList = [];

  @observable sprintList = [];

  @observable resizing = false;

  @observable storyMapData = {};

  issueSearchStore = null;

  setIssueSearchStore(issueSearchStore) {
    this.issueSearchStore = issueSearchStore;
  }

  @action setStoryMapData = (data) => {
    this.storyMapData = data;
  }

  @observable storyData = {};

  @action setStoryData = (data) => {
    this.storyData = {};
  };

  @observable loading = false;

  @observable selectedIssueMap = observable.map({});

  @observable hiddenColumnNoStory = false;

  @action setHiddenColumnNoStory = (data) => {
    this.hiddenColumnNoStory = data;
  }

  @observable foldCompletedEpic = false;

  @action setFoldCompletedEpic = (data) => {
    this.foldCompletedEpic = data;
  }

  @observable sideIssueListPageInfo = {
    page: 1,
    size: 100,
    hasNextPage: false,
  };

  @computed get getSideIssueListPageInfo() {
    return this.sideIssueListPageInfo;
  }

  @action setSideIssueListPageInfo(pageInfo) {
    this.sideIssueListPageInfo = pageInfo;
  }

  miniMap = {};

  @action clearData = () => {
    this.storyMapData = {};
    this.storyData = {};
  }

  @action clear() {
    this.storyMapData = {};
    this.storyData = {};
    this.searchVO = {
      advancedSearchArgs: {},
      otherArgs: {
        customField: {
          option: [], date: [], date_hms: [], number: [], string: [], text: [],
        },
      },
      searchArgs: {},
    };
    this.versionList = [];
    this.sprintList = [];
    this.selectedIssueMap.clear();
    this.hiddenColumnNoStory = false;
    this.foldCompletedEpic = false;
    this.epicInViewportMap = observable.map();
    this.rowInViewportMap = observable.map();
  }

  @action resetSearchVO() {
    this.searchVO = {
      advancedSearchArgs: {},
      otherArgs: {
        customField: {
          option: [], date: [], date_hms: [], number: [], string: [], text: [],
        },
      },
      searchArgs: {},
    };
  }

  @action initStoryMapData = (storyMapData, firstLoad) => {
    let epicWithFeature = storyMapData.epics || storyMapData.epicWithFeature;
    const { featureWithoutEpic = [] } = storyMapData;
    epicWithFeature = sortBy(epicWithFeature, 'epicRank');
    const newStoryMapData = {
      ...storyMapData,
      epicWithFeature: featureWithoutEpic.length > 0 ? epicWithFeature.map((epic) => ({ ...epic, featureCommonDTOList: epic.featureCommonDTOList || [] })).concat({
        issueId: 0,
        featureCommonDTOList: featureWithoutEpic,
      }) : epicWithFeature.map((epic) => ({ ...epic, featureCommonDTOList: epic.featureCommonDTOList || [] })),
    };

    this.initStoryData(newStoryMapData, firstLoad);
  }

  getStoryMap = async (firstLoad = false) => {
    this.setLoading(true);
    const searchVO = cloneDeep(toJS(this.searchVO));
    let versionList = (this.allVersion || []).slice(0, 5);
    let sprintIds = (this.allSprints || []).slice(0, 5).map((s) => s.sprintId);
    if (this.swimLine === 'version') {
      if (!this.allVersion) {
        const allVersion = await versionApi.loadNamesByStatus();
        const planingVersions = allVersion.filter((v) => v.statusCode === 'version_planning');
        const otherVersions = allVersion.filter((v) => v.statusCode !== 'version_planning');
        this.allVersion = [...planingVersions.reverse(), ...otherVersions];
      }
      versionList = this.allVersion.slice(0, 5);
      if (versionList.length > 0) {
        if (!searchVO.otherArgs.version || searchVO.otherArgs.version.length <= 0) {
          searchVO.otherArgs.version = versionList.map((v) => v.versionId);
        }
        versionList = this.allVersion.filter((version) => searchVO.otherArgs.version.includes(version.versionId));
      }
    } else if (this.swimLine === 'sprint') {
      if (!this.allSprints) {
        const allSprints = await sprintApi.loadSprints();
        const planingSprints = allSprints.filter((v) => v.statusCode === 'sprint_planning').reverse();
        const otherSprints = allSprints.filter((v) => v.statusCode !== 'sprint_planning');
        this.allSprints = [...planingSprints, ...otherSprints];
      }
      sprintIds = this.allSprints.slice(0, 5).map((s) => s.sprintId);
      if (sprintIds.length > 0) {
        if (!searchVO.otherArgs.sprint || searchVO.otherArgs.sprint.length <= 0) {
          searchVO.otherArgs.sprint = sprintIds;
        }
        sprintIds = searchVO.otherArgs.sprint;
      }
    }

    Promise.all([
      storyMapApi.getStoryMap(searchVO),
      issueTypeApi.loadAllWithStateMachineId(),
      priorityApi.loadByProject(),
      sprintApi.loadSprintsWidthInfo(sprintIds),
    ]).then(([storyMapData, issueTypes, prioritys, sprintList]) => {
      this.issueTypes = issueTypes;
      this.prioritys = prioritys;
      this.initVersionList(versionList, this.swimLine === 'version' ? searchVO.otherArgs.version && searchVO.otherArgs.version.includes('0') : true);
      this.initSprintList(sprintList || [], this.swimLine === 'sprint' ? searchVO.otherArgs.sprint && searchVO.otherArgs.sprint.includes('0') : true);
      this.initStoryMapData(storyMapData, firstLoad);
      this.setLoading(false);
    }).catch((error) => {
      this.setLoading(false);
    });
  }

  loadIssueList = (propsPage = 1) => {
    const { page = 1, size = 100 } = this.sideIssueListPageInfo || {};
    const newPage = propsPage ?? page;
    storyMapApi.getDemands(this.sideSearchVO, newPage, size).then((res) => {
      const list = res.isFirstPage ? res.content : this.issueList.concat(res.content);
      this.setIssueList(list);
      this.setSideIssueListPageInfo({ page: res.pageNum, size: res.pageSize, hasNextPage: res.hasNextPage });
    });
  }

  @action handleSideFilterChange = (field, values) => {
    this.sideSearchVO.advancedSearchArgs[field] = values;
    this.loadIssueList();
  }

  clearSideFilter = () => {
    this.sideSearchVO = {
      searchArgs: {
        assigneeId: null,
      },
      advancedSearchArgs: {
        versionList: [],
        statusList: [],
      },
    };
  }

  @action setIssueList(issueList) {
    this.issueList = issueList;
  }

  @action setSideIssueListVisible(sideIssueListVisible) {
    this.sideIssueListVisible = sideIssueListVisible;
  }

  @action setCreateModalVisible(createModalVisible) {
    this.createModalVisible = createModalVisible;
  }

  @action setCreateEpicModalVisible(createEpicModalVisible) {
    this.createEpicModalVisible = createEpicModalVisible;
  }

  @action setCreateFeatureModalVisible(createFeatureModalVisible) {
    this.createFeatureModalVisible = createFeatureModalVisible;
  }

  @action toggleSideIssueListVisible(visible) {
    // 关闭Issue详情侧边
    if (!visible) {
      this.setClickIssue();
    }

    this.sideIssueListVisible = visible;
  }

  @action setIsFullScreen(isFullScreen) {
    this.isFullScreen = isFullScreen;
  }

  @action setLoading(loading) {
    this.loading = loading;
  }

  @action switchSwimLine(swimLine) {
    this.clearData();
    this.swimLine = swimLine;
    localStorage.setItem('agile.StoryMap.SwimLine', swimLine);
    this.getStoryMap();
  }

  @action initVersionList(versionList, hasNoPlan) {
    this.versionList = (hasNoPlan ? versionList.concat([{
      versionId: 'none',
      name: '未计划部分',
    }]) : versionList).map((version) => {
      const oldVersion = find(this.versionList, { versionId: version.versionId });
      if (oldVersion) {
        return { ...version, storyNum: 0, collapse: oldVersion.collapse };
      }
      return { ...version, storyNum: 0, collapse: false };
    });
  }

  @action initSprintList(sprintList, hasNoPlan) {
    this.sprintList = (hasNoPlan ? sprintList.concat([{
      sprintId: 'none',
      sprintName: '未计划部分',
    }]) : sprintList).map((sprint) => {
      const oldSprint = find(this.sprintList, { sprintId: sprint.sprintId });
      if (oldSprint) {
        return { ...sprint, storyNum: 0, collapse: oldSprint.collapse };
      }
      return { ...sprint, storyNum: 0, collapse: false };
    });
  }

  @action resetVersionList() {
    this.versionList = this.versionList.map((version) => ({
      ...version,
      storyNum: 0,
    }));
  }

  @action resetSprintList() {
    this.sprintList = this.sprintList.map((sprint) => (
      {
        ...sprint,
        storyNum: 0,
      }
    ));
  }

  getInitVersions() {
    const versionObj = {};
    this.versionList.forEach((version) => {
      versionObj[version.versionId] = [];
    });
    return versionObj;
  }

  getInitVersions() {
    const versionObj = {};
    this.versionList.forEach((version) => {
      versionObj[version.versionId] = [];
    });
    return versionObj;
  }

  getInitSprints() {
    const sprintObj = {};
    this.sprintList.forEach((sprint) => {
      sprintObj[sprint.sprintId] = [];
    });
    return sprintObj;
  }

  @action initStoryData(storyMapData, firstLoad) {
    const {
      epicWithFeature, storyList, storyMapWidth,
    } = storyMapData;
    const storyData = {};
    epicWithFeature.forEach((epic) => {
      const { issueId: epicId } = epic;
      const epicWithWidth = find(storyMapWidth, { issueId: epic.issueId, type: 'epic' });
      storyData[epicId] = {
        epicId,
        // eslint-disable-next-line no-nested-ternary
        collapse: (this.foldCompletedEpic && epic.statusVO?.completed) || this.storyData[epicId]?.collapse,
        storys: [],
        feature: epicId ? { // 无史诗不显示无特性
          none: {
            storys: [],
            version: this.getInitVersions(),
            sprint: this.getInitSprints(),
            width: epicWithWidth ? epicWithWidth.width : 1,
          },
        } : {},
      };
      const targetFeature = storyData[epicId].feature;
      // eslint-disable-next-line no-unused-expressions
      epic.featureCommonDTOList && epic.featureCommonDTOList.forEach((feature) => {
        if (feature.issueId && !targetFeature[feature.issueId]) {
          const featureWithWidth = find(storyMapWidth, { issueId: feature.issueId, type: 'feature' });
          targetFeature[feature.issueId] = {
            storys: [],
            version: this.getInitVersions(),
            sprint: this.getInitSprints(),
            width: featureWithWidth ? featureWithWidth.width : 1,
          };
        }
      });
    });
    storyList.forEach((story) => {
      this.addStoryToStoryData(story, storyData);
    });
    this.storyData = storyData;
    this.storyMapData = storyMapData;
    if (firstLoad) {
      const defaultFoldCompletedEpic = localPageCacheStore.getItem('stroyMap.fold.completedEpic');
      defaultFoldCompletedEpic && this.foldCompletedEpicColumn(defaultFoldCompletedEpic);
    }
  }

  @action foldCompletedEpicColumn(fold) {
    const {
      epicWithFeature = [],
    } = this.storyMapData;
    epicWithFeature.forEach((epic) => {
      const { issueId: epicId, statusVO = {} } = epic;
      const { completed } = statusVO;
      if (epicId) {
        this.storyData[epicId] = {
          ...this.storyData[epicId],
          collapse: completed && fold,
        };
      }
    });
  }

  @action addStoryToStoryData(story, storyData = this.storyData) {
    const {
      epicId, featureId, storyMapVersionDTOList, storyMapSprintList,
    } = story;
    if (epicId !== undefined && storyData[epicId] && storyData.epicId === story.epicData) {
      const targetEpic = storyData[epicId];
      const { feature, storys } = targetEpic;
      storys.push(story);
      const targetFeature = feature[featureId || 'none'];
      if (targetFeature) {
        targetFeature.storys.push(story);
        // 故事按照version泳道分类
        // if (this.swimLine === 'version') {
        if (storyMapVersionDTOList.length === 0) {
          this.addStoryNumToVersion('none');
          if (!targetFeature.version.none) {
            targetFeature.version.none = [];
          }
          targetFeature.version.none.push(story);
        }
        storyMapVersionDTOList.forEach((version) => {
          const { versionId } = version;
          // if (!targetFeature.version[versionId]) {
          //   set(targetFeature.version, {
          //     [versionId]: [],
          //   });
          // }
          this.addStoryNumToVersion(versionId);
          if (!targetFeature.version[versionId]) {
            targetFeature.version[versionId] = [];
          }
          targetFeature.version[versionId].push(story);
        });

        // 冲刺
        if (!storyMapSprintList || storyMapSprintList.length === 0) {
          this.addStoryNumToSprint('none');
          if (!targetFeature.sprint.none) {
            targetFeature.sprint.none = [];
          }
          targetFeature.sprint.none.push(story);
        }
        (storyMapSprintList || []).forEach((sprint) => {
          const { sprintId } = sprint;
          this.addStoryNumToSprint(sprintId);
          if (!targetFeature.sprint[sprintId]) {
            targetFeature.sprint[sprintId] = [];
          }
          targetFeature.sprint[sprintId].push(story);
        });
      }

      // }
    }
  }

  @action addStoryNumToVersion(versionId) {
    const version = find(this.versionList, { versionId });
    if (version) {
      version.storyNum += 1;
    }
  }

  @action addStoryNumToSprint(sprintId) {
    const sprint = find(this.sprintList, { sprintId });
    if (sprint) {
      sprint.storyNum += 1;
    }
  }

  @action collapse(epicId) {
    this.storyData[epicId].collapse = !this.storyData[epicId].collapse;
  }

  @action collapseStory(id) {
    switch (this.swimLine) {
      case 'version': {
        const targetVersion = find(this.versionList, { versionId: id });
        targetVersion.collapse = !targetVersion.collapse;
        break;
      }
      case 'sprint': {
        const targetSprint = find(this.sprintList, { sprintId: id });
        targetSprint.collapse = !targetSprint.collapse;
        break;
      }
      default: break;
    }
  }

  @action addEpic(epicData) {
    const epic = {
      adding: true,
      featureCommonDTOList: [],
    };
    // 删掉之前正在创建的
    this.removeAddingEpic();
    const currentIndex = findIndex(this.storyMapData.epicWithFeature, { issueId: epicData.issueId });
    // console.log(currentIndex);
    this.storyMapData.epicWithFeature.splice(currentIndex + 1, 0, epic);
  }

  @action removeAddingEpic() {
    remove(this.storyMapData.epicWithFeature, { adding: true });
  }

  @action removeAddingFeature(epicId) {
    const targetEpic = find(this.storyMapData.epicWithFeature, { issueId: epicId });
    if (targetEpic) {
      remove(targetEpic.featureCommonDTOList, { adding: true });
    }
  }

  @action afterCreateEpic(index, newEpic) {
    this.storyMapData.epicWithFeature[index] = { ...newEpic, featureCommonDTOList: [] };
    set(this.storyData, {
      [newEpic.issueId]: {
        epicId: newEpic.issueId,
        collapse: false,
        storys: [],
        feature: {
          none: {
            storys: [],
            version: this.getInitVersions(),
            sprint: this.getInitSprints(),
            width: 1,
          },
        },
      },
    });
  }

  @action afterCreateEpicInModal(newEpic) {
    this.storyMapData.epicWithFeature.unshift({ ...newEpic, featureCommonDTOList: [] });
    set(this.storyData, {
      [newEpic.issueId]: {
        epicId: newEpic.issueId,
        collapse: false,
        storys: [],
        feature: {
          none: {
            storys: [],
            version: this.getInitVersions(),
            sprint: this.getInitSprints(),
            width: 1,
          },
        },
      },
    });
  }

  @action afterEditEpicName(newEpic) {
    this.storyMapData.epicWithFeature = this.storyMapData.epicWithFeature.map((epic) => {
      if (epic.issueId === newEpic.issueId) {
        return ({
          ...epic,
          epicName: newEpic.epicName,
          objectVersionNumber: epic.objectVersionNumber + 1,
        });
      }
      return epic;
    });
  }

  @action addFeature(epic) {
    const feature = {
      adding: true,
    };

    const currentIndex = findIndex(this.storyMapData.epicWithFeature, { issueId: epic.issueId });
    // console.log(currentIndex);
    // console.log(epic, currentIndex);
    this.storyMapData.epicWithFeature[currentIndex].featureCommonDTOList.push(feature);
  }

  @action afterCreateFeature(epicIndex, newFeature) {
    const { length } = this.storyMapData.epicWithFeature[epicIndex].featureCommonDTOList;
    this.storyMapData.epicWithFeature[epicIndex].featureCommonDTOList[length - 1] = newFeature;
    const { issueId: epicId } = this.storyMapData.epicWithFeature[epicIndex];
    set(this.storyData[epicId].feature, {
      [newFeature.issueId]: {
        storys: [],
        version: this.getInitVersions(),
        sprint: this.getInitSprints(),
        width: 1,
      },
    });
  }

  @action afterCreateStory(newStory) {
    this.addStoryToStoryData(newStory);
    this.storyMapData.storyList.push(newStory);
  }

  @action removeStoryFromStoryMap(story, targetVersionOrSprintId) {
    const {
      epicId, featureId, storyMapVersionDTOList, storyMapSprintList,
    } = story;
    if (targetVersionOrSprintId) {
      this.getStoryMap();
      this.setClickIssue();
      // if (this.storyData[epicId]) {
      //   const targetEpic = this.storyData[epicId];
      //   const { feature } = targetEpic;
      //   const targetFeature = feature[featureId || 'none'];
      //   remove(targetFeature.version[targetVersionId], { issueId: story.issueId });
      //   remove(storyMapVersionDTOList, { versionId: targetVersionId });
      //   // 版本全删掉后，移到未规划
      //   if (story.storyMapVersionDTOList.length === 0) {
      //     targetFeature.version.none.push(story);
      //   }
      // }
    } else {
      remove(this.storyMapData.storyList, { issueId: story.issueId });
      if (this.storyData[epicId]) {
        const targetEpic = this.storyData[epicId];
        const { feature } = targetEpic;
        remove(targetEpic.storys, { issueId: story.issueId });
        const targetFeature = feature[featureId || 'none'];
        remove(targetFeature.storys, { issueId: story.issueId });
        // 从各个版本移除
        if (storyMapVersionDTOList.length === 0) {
          if (targetFeature.version.none) {
            remove(targetFeature.version.none, { issueId: story.issueId });
            const version = find(this.versionList, { versionId: 'none' });
            if (version) {
              version.storyNum -= 1;
            }
          }
          storyMapVersionDTOList.forEach((version) => {
            const { versionId } = version;
            remove(targetFeature.version[versionId], { issueId: story.issueId });
            const v = find(this.versionList, { versionId });
            if (version) {
              v.storyNum -= 1;
            }
          });
          // 从各个冲刺移除
          if (storyMapSprintList.length === 0) {
            if (targetFeature.sprint.none) {
              remove(targetFeature.sprint.none, { issueId: story.issueId });
              const sprint = find(this.sprintList, { sprintId: 'none' });
              if (sprint) {
                sprint.storyNum -= 1;
              }
            }
          }
          storyMapSprintList.forEach((sprint) => {
            const { sprintId } = sprint;
            remove(targetFeature.sprint[sprintId], { issueId: story.issueId });
            const v = find(this.sprintList, { sprintId });
            if (sprint && v) {
              v.storyNum -= 1;
            }
          });
        }
      }
    }
  }

  @action setFeatureWidth({
    epicId,
    featureId,
    width,
  }) {
    this.storyData[epicId].feature[featureId].width = width;
  }

  changeWidth({
    width,
    issueId,
    type,
  }, {
    epicId,
    featureId,
    initWidth,
  }) {
    const { storyMapWidth } = this.storyMapData;
    const targetWidth = find(storyMapWidth, { type, issueId });
    const targetIndex = findIndex(storyMapWidth, { type, issueId });
    const storyMapWidthVO = {
      ...targetWidth,
      projectId: getProjectId(),
      width,
      issueId,
      type,
    };
    if (!targetWidth) {
      storyMapApi.createWidth(storyMapWidthVO).then((res) => {
        if (res.failed) {
          this.setFeatureWidth({
            epicId,
            featureId,
            width: initWidth,
          });
        } else {
          this.addWidthVO(res);
        }
      }).catch((err) => {
        this.setFeatureWidth({
          epicId,
          featureId,
          width: initWidth,
        });
      });
    } else {
      storyMapApi.changeWidth(storyMapWidthVO).then((res) => {
        if (res.failed) {
          this.setFeatureWidth({
            epicId,
            featureId,
            width: initWidth,
          });
        } else {
          action(() => {
            storyMapWidth[targetIndex] = res;
          })();
        }
      }).catch((err) => {
        this.setFeatureWidth({
          epicId,
          featureId,
          width: initWidth,
        });
      });
    }
  }

  @action addWidthVO(storyMapWidthVO) {
    const { storyMapWidth } = this.storyMapData;
    storyMapWidth.push(storyMapWidthVO);
  }

  @action setClickIssue(clickIssue) {
    const setData = () => {
      this.selectedIssueMap.clear();
      if (clickIssue) {
        this.sideIssueListVisible = false;
        this.selectedIssueMap.set(clickIssue.issueId, clickIssue);
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

  sortEpic(source, destination, sourceIndex, resultIndex) {
    if (!source || !destination || source.issueId === destination.issueId) {
      return;
    }
    const sortVO = {
      projectId: getProjectId(),
      objectVersionNumber: source.epicRankObjectVersionNumber, // 乐观锁
      issueId: source.issueId,
      type: 'epic',
      before: true, // 是否拖动到第一个
      after: false,
      referenceIssueId: destination.issueId,
    };

    storyMapApi.sort(sortVO).then(() => {
      const [removed] = this.storyMapData.epicWithFeature.splice(sourceIndex, 1);
      this.storyMapData.epicWithFeature.splice(resultIndex, 0, removed);
    });
  }

  getIssueTypeByCode(typeCode) {
    return find(this.issueTypes, { typeCode });
  }

  setMiniMapRef(ref) {
    this.miniMap = ref;
  }

  @computed get getEpicList() {
    const { epicWithFeature } = this.storyMapData || {};
    return epicWithFeature || [];
  }

  @computed get getIsEmpty() {
    const { epicWithFeature, featureWithoutEpic } = this.storyMapData;
    if (epicWithFeature && featureWithoutEpic) {
      return featureWithoutEpic.length === 0 && epicWithFeature.filter((epic) => epic.issueId).length === 0;
    } if (epicWithFeature) {
      return epicWithFeature.filter((epic) => epic.issueId).length === 0;
    }
    return false;
  }

  @computed get getEpicType() {
    return find(this.issueTypes, { typeCode: 'issue_epic' });
  }

  @computed get getFeatureType() {
    return find(this.issueTypes, { typeCode: 'feature' });
  }

  @computed get getDefaultPriority() {
    return find(this.prioritys, { default: true }) || this.prioritys[0];
  }

  @observable tableOverflow = false;

  @action setTableOverflow({ tableWidth = 0, containerWidth = 0 }) {
    this.tableOverflow = tableWidth > containerWidth;
  }

  // 筛选列表是否显示
  @observable filterListVisible = false;

  @computed get getFilterListVisible() {
    return this.filterListVisible;
  }

  @action setFilterListVisible = (data) => {
    this.filterListVisible = data;
  }

  @observable epicInViewportMap = observable.map();

  @action setEpicInViewportMap = (key, value) => {
    this.epicInViewportMap.set(key, value);
  }

  @observable rowInViewportMap = observable.map();

  @action setRowInViewportMap = (key, value) => {
    this.rowInViewportMap.set(key, value);
  }

  @observable tableWidth = 0;

  @action setTableWidth = (data) => {
    this.tableWidth = data;
  }

  @observable scrollWidth = 0;

  @action setScrollWidth = (data) => {
    this.scrollWidth = data;
  }

  @observable pageDataMap = observable.map();

  @observable detailProps = {};

  @action setDetailProps = (data) => {
    this.detailProps = data;
  }
}

export default new StoryMapStore();
