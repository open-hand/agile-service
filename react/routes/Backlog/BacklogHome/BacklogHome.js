/* eslint-disable react/no-unused-state */
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { Link } from 'react-router-dom';
import {
  TabPage as Page, Header, stores, Breadcrumb, Content,
} from '@choerodon/boot';
import { DragDropContext } from 'react-beautiful-dnd';
import {
  Button, Spin, Checkbox, Icon,
} from 'choerodon-ui';
import { Modal } from 'choerodon-ui/pro';
import Version from '../components/VersionComponent/Version';
import Epic from '../components/EpicComponent/Epic';
import IssueDetail from '../components/IssueDetailComponent/IssueDetail';
import CreateIssue from '../../../components/CreateIssue';
import './BacklogHome.less';
import SprintItem from '../components/SprintComponent/SprintItem';
import CreateSprint from '../components/create-sprint';
import Injecter from '../../../components/Injecter';

const { AppState } = stores;
const createSprintKey = Modal.key();
@inject('HeaderStore')
@observer
class BacklogHome extends Component {
  constructor(props) {
    super(props);
    this.state = {
      spinIf: false,
      versionVisible: false,
      epicVisible: false,
      display: false,
      first: true,
    };  
  }

  componentDidMount() {
    this.refresh();
  }

  componentWillUnmount() {
    const { BacklogStore } = this.props;
    BacklogStore.resetData();
    BacklogStore.clearMultiSelected();
    BacklogStore.resetFilter();
  }

  /**
   * 加载选择快速搜索的冲刺数据
   */
  getSprint = () => {
    const { BacklogStore } = this.props;
    BacklogStore.axiosGetIssueTypes();
    BacklogStore.axiosGetDefaultPriority();
    Promise.all([BacklogStore.axiosGetQuickSearchList(), BacklogStore.axiosGetIssueTypes(), BacklogStore.axiosGetDefaultPriority(), BacklogStore.axiosGetSprint()]).then(([quickSearch, issueTypes, priorityArr, backlogData]) => {
      BacklogStore.initBacklogData(quickSearch, issueTypes, priorityArr, backlogData);
      this.setState({
        first: false,
      });
    });
  };

  /**
   * 加载版本数据
   */
  loadVersion = () => {
    const { BacklogStore } = this.props;
    BacklogStore.axiosGetVersion().then((data2) => {
      const newVersion = [...data2];
      for (let index = 0, len = newVersion.length; index < len; index += 1) {
        newVersion[index].expand = false;
      }
      BacklogStore.setVersionData(newVersion);
    }).catch((error) => {
    });
  };

  /**
   * 加载史诗
   */
  loadEpic = () => {
    const { BacklogStore } = this.props;
    BacklogStore.axiosGetEpic().then((data3) => {
      const newEpic = [...data3];
      for (let index = 0, len = newEpic.length; index < len; index += 1) {
        newEpic[index].expand = false;
      }
      BacklogStore.setEpicData(newEpic);
    }).catch((error3) => {
    });
  };

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

  refresh = (spinIf = true) => {
    const { BacklogStore } = this.props;
    if (this.IssueDetail) {
      this.IssueDetail.refreshIssueDetail();
    }
    if (spinIf) {
      BacklogStore.setSpinIf(true);
    }
    this.getSprint();
    if (BacklogStore.getCurrentVisible === 'version') {
      this.loadVersion();
    } else if (BacklogStore.getCurrentVisible === 'epic') {
      this.loadEpic();
    }
  };

  /**
   * 加载快速搜索
   */
  loadQuickFilter = () => {

  };

  /**
   * 创建冲刺
   */
  handleCreateSprint = () => {
    const { BacklogStore } = this.props;
    const onCreate = (sprint) => {
      BacklogStore.setCreatedSprint(sprint.sprintId);
      this.refresh();
    };
    Modal.open({
      drawer: true,
      style: {
        width: 340,
      },
      key: createSprintKey,
      title: '创建冲刺',
      children: <CreateSprint onCreate={onCreate} />,
    });
  };

  resetSprintChose = () => {
    this.resetMuilterChose();
  };

  /**
   * issue详情回退关闭详情侧边栏
   */
  resetMuilterChose = () => {
    this.setState({
      selected: {
        droppableId: '',
        issueIds: [],
      },
    });
  };

  onQuickSearchChange = (onlyMeChecked, onlyStoryChecked, moreChecked) => {
    const { BacklogStore } = this.props;
    BacklogStore.setQuickFilters(onlyMeChecked, onlyStoryChecked, moreChecked);
    BacklogStore.axiosGetSprint()
      .then((res) => {
        BacklogStore.setSprintData(res);
      }).catch((error) => {
      });
  }

  onAssigneeChange = (data) => {
    const { BacklogStore } = this.props;
    BacklogStore.setAssigneeFilterIds(data);
    BacklogStore.axiosGetSprint()
      .then((res) => {
        BacklogStore.setSprintData(res);
        this.setState({
          spinIf: false,
        });
      }).catch((error) => {
      });
  };

  handleClickCBtn = () => {
    const { BacklogStore } = this.props;
    BacklogStore.setNewIssueVisible(true);
  }

  handleCreateIssue = (res) => {
    const { BacklogStore } = this.props;
    BacklogStore.setNewIssueVisible(false);
    // 创建issue后刷新
    if (res) {
      this.refresh(false, res);
    }
  };

  toggleCurrentVisible = (type) => {
    const { BacklogStore } = this.props;
    const currentVisible = BacklogStore.getCurrentVisible;
    if (currentVisible === type) {
      BacklogStore.toggleVisible(null);
    } else {
      BacklogStore.toggleVisible(type);
    }
  };

  onCheckChange = (e) => {
    this.setState({
      display: e.target.checked,
    });
  };

  render() {
    const { BacklogStore } = this.props;
    const arr = BacklogStore.getSprintData;
    const { display } = this.state;
    const {
      name, type, id, organizationId: orgId,
    } = AppState.currentMenuType;
    return (
      <Page
        service={[
          // 'agile-service.product-version.createVersion',
          'agile-service.issue.deleteIssue',
          'agile-service.sprint.queryByProjectId',
        ]}
        className="c7n-backlog-page"
      >
        <Header title="待办事项">
          <Button
            className="leftBtn"
            funcType="flat"
            onClick={this.handleClickCBtn}
          >
            <Icon type="playlist_add icon" />
            <span>创建问题</span>
          </Button>
          <Button className="leftBtn" functyp="flat" onClick={this.handleCreateSprint}>
            <Icon type="playlist_add icon" />
            创建冲刺
          </Button>
          {/* <Button
            className="leftBtn2"
            functyp="flat"
            onClick={() => {
              this.refresh();
              this.loadQuickFilter();
            }}
          >
            <Icon type="refresh" />
            {'刷新'}
          </Button> */}
          {false && arr.length && arr.length > 1
            ? (
              <Checkbox
                className="primary"
                style={{ marginLeft: 20 }}
                onChange={this.onCheckChange}
              >
                显示未开始冲刺
              </Checkbox>
            ) : ''
          }
          {/* <div style={{
            height: '50%', width: 1, background: 'rgba(0,0,0,0.12', margin: '0 30px',
          }}
          />
          <QuickSearch
            hideQuickSearch={BacklogStore.getCurrentVisible === 'feature'}
            onQuickSearchChange={this.onQuickSearchChange}
            resetFilter={BacklogStore.getQuickSearchClean}
            onAssigneeChange={this.onAssigneeChange}
          /> */}
          {/* <ClearFilter /> */}
        </Header>
        <Breadcrumb />
        {/* 盖住tab下面的边框 */}
        <div style={{
          width: '100%',
          position: 'absolute',
          background: 'white',
          height: '1px',
          top: '137px',
          left: '109px',
          zIndex: 1,
        }}
        />
        <Content style={{
          padding: 0, paddingTop: 4, display: 'flex', flexDirection: 'column',
        }}
        >
          <div
            className="c7n-backlog"
            style={{
              flex: 1,
              overflow: 'hidden',
            }}
          >
            <div className="c7n-backlog-side">
              <p
                role="none"
                onClick={() => {
                  this.toggleCurrentVisible('version');
                }}
              >
                版本
              </p>
              <p
                style={{
                  marginTop: 12,
                }}
                role="none"
                onClick={() => {
                  this.toggleCurrentVisible('epic');
                }}
              >
                史诗
              </p>
            </div>
            <Version
              store={BacklogStore}
              refresh={this.refresh}
              visible={BacklogStore.getCurrentVisible}
              issueRefresh={() => {
                this.IssueDetail.refreshIssueDetail();
              }}
            />
            <Epic
              refresh={this.refresh}
              visible={BacklogStore.getCurrentVisible}
              issueRefresh={() => {
                this.IssueDetail.refreshIssueDetail();
              }}
            />
            <Spin spinning={BacklogStore.getSpinIf}>
              <div className="c7n-backlog-content">
                <DragDropContext
                  onDragEnd={(result) => {
                    // console.log('onDragEnd', result);
                    BacklogStore.setIsDragging(null);
                    const { destination, source, draggableId } = result;
                    if (destination) {
                      const { droppableId: destinationId, index: destinationIndex } = destination;
                      const { droppableId: sourceId, index: sourceIndex } = source;
                      if (destinationId === sourceId && destinationIndex === sourceIndex) {
                        return;
                      }
                      if (result.reason !== 'CANCEL') {
                        const item = BacklogStore.getIssueMap.get(sourceId)[sourceIndex];
                        const destinationArr = BacklogStore.getIssueMap.get(destinationId);
                        let destinationItem;
                        if (destinationIndex === 0) {
                          destinationItem = null;
                        } else if (destinationIndex === BacklogStore.getIssueMap.get(destinationId).length) {
                          destinationItem = destinationArr[destinationIndex - 1];
                        } else {
                          destinationItem = destinationArr[destinationIndex];
                        }
                        if (BacklogStore.getMultiSelected.size > 1 && !BacklogStore.getMultiSelected.has(destinationItem)) {
                          BacklogStore.moveSingleIssue(destinationId, destinationIndex, sourceId, sourceIndex, draggableId, item, 'multi').then(() => {
                            if (this.IssueDetail) {
                              this.IssueDetail.refreshIssueDetail();
                            }
                          });
                        } else {
                          BacklogStore.moveSingleIssue(destinationId, destinationIndex, sourceId, sourceIndex, draggableId, item, 'single').then(() => {
                            if (this.IssueDetail) {
                              this.IssueDetail.refreshIssueDetail();
                            }
                          });
                        }
                      }
                    }
                  }}
                  onDragStart={(result) => {
                    const { source } = result;
                    const { droppableId: sourceId, index: sourceIndex } = source;
                    const item = BacklogStore.getIssueMap.get(sourceId)[sourceIndex];
                    BacklogStore.setIsDragging(item.issueId);
                    BacklogStore.setIssueWithEpicOrVersion(item);
                  }}
                >
                  <SprintItem
                    first={this.state.first}
                    display={display}
                    epicVisible={BacklogStore.getEpicVisible}
                    versionVisible={BacklogStore.getVersionVisible}
                    onAssigneeChange={this.onAssigneeChange}
                    onRef={(ref) => {
                      this.sprintItemRef = ref;
                    }}
                    refresh={this.refresh}
                  />
                </DragDropContext>
                <Injecter store={BacklogStore} item="newIssueVisible">
                  {visible => (
                    visible
                      ? (
                        <CreateIssue
                          visible={visible}
                          onCancel={() => {
                            BacklogStore.setNewIssueVisible(false);
                          }}
                          onOk={this.handleCreateIssue}
                        />
                      ) : ''
                  )}
                </Injecter>
              </div>
            </Spin>
            <IssueDetail
              // refresh={() => this.refresh.bind(false)}
              refresh={() => this.refresh(false)}
              onRef={(ref) => {
                this.IssueDetail = ref;
              }}
              cancelCallback={this.resetSprintChose}
            />
          </div>
        </Content>
      </Page>
    );
  }
}

export default BacklogHome;
