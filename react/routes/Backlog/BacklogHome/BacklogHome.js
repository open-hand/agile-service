import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import {
  TabPage as Page, Header, Breadcrumb, Content,
  Permission,
} from '@choerodon/boot';
import {
  Button, Spin, Icon, Tooltip,
} from 'choerodon-ui';
import { Modal } from 'choerodon-ui/pro';

import Version from '../components/VersionComponent/Version';
import Epic from '../components/EpicComponent/Epic';
import Feature from '../components/FeatureComponent/Feature';
import IssueDetail from '../components/issue-detail';
import CreateIssue from '../components/create-issue';
import CreateSprint, { CreateCurrentPiSprint } from '../components/create-sprint';
import SprintList from '../components/sprint-list';
import ShowPlanSprint from '../components/show-plan-sprint';
import './BacklogHome.less';

const createSprintKey = Modal.key();
const createCurrentPiSprintKey = Modal.key();

@observer
class BacklogHome extends Component {
  componentDidMount() {
    const { BacklogStore } = this.props;
    BacklogStore.resetData();
    BacklogStore.refresh();
    const { isShowFeature } = this.props;
    if (isShowFeature) {
      BacklogStore.loadPiInfoAndSprint();
    }
  }

  refresh = (...args) => {
    const { BacklogStore } = this.props;
    BacklogStore.refresh(...args);
  }

  componentWillUnmount() {
    const { BacklogStore } = this.props;
    BacklogStore.resetData();
    BacklogStore.clearMultiSelected();
    BacklogStore.resetFilter();
  }

  /**
   * 创建冲刺
   */
  handleCreateSprint = async () => {
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

  /**
   * 当前PI下创建冲刺
   */
  handleCreateCurrentPiSprint = async () => {
    const { BacklogStore } = this.props;
    await BacklogStore.loadPiInfoAndSprint();
    const onCreate = (sprint) => {
      BacklogStore.setCreatedSprint(sprint.sprintId);
      this.refresh();
    };
    const piInfo = BacklogStore.getPiInfo;
    const sprints = BacklogStore.getSprints;
    Modal.open({
      drawer: true,
      style: {
        width: 340,
      },
      key: createCurrentPiSprintKey,
      title: '当前PI下创建冲刺',
      children: <CreateCurrentPiSprint onCreate={onCreate} PiName={`${piInfo.code}-${piInfo.name}`} sprints={sprints} piId={piInfo.id} />,
    });
  };

  handleClickCBtn = () => {
    const { BacklogStore } = this.props;
    BacklogStore.setNewIssueVisible(true);
  }

  toggleCurrentVisible = (type) => {
    const { BacklogStore } = this.props;
    const currentVisible = BacklogStore.getCurrentVisible;
    if (currentVisible === type) {
      BacklogStore.toggleVisible(null);
    } else {
      BacklogStore.toggleVisible(type);
      if (type === 'feature') {
        BacklogStore.clearMultiSelected();
      }
    }
  };

  renderCreateSprintInPi = (visible, disabled) => {
    if (!visible) {
      return null;
    }
    return disabled ? (
      <Tooltip title="无活跃的PI">
        <Button icon="playlist_add" disabled>
          当前PI下创建冲刺
        </Button>
      </Tooltip>
    ) : (
      <Permission
        service={['choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint']}
      >
        <Button icon="playlist_add" onClick={this.handleCreateCurrentPiSprint}>
          当前PI下创建冲刺
        </Button>
      </Permission>
    );
  }

  render() {
    const { BacklogStore } = this.props;
    const arr = BacklogStore.getSprintData;
    const { isInProgram, isShowFeature } = this.props;
    return (
      <>
        <Header title="待办事项">
          <Button
            onClick={this.handleClickCBtn}
            icon="playlist_add"
          >
            创建问题
          </Button>
          {!isShowFeature && (
            <Permission
              service={['choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint']}
            >
              <Button icon="playlist_add" onClick={this.handleCreateSprint}>
                创建冲刺
              </Button>
            </Permission>
          )}
          {this.renderCreateSprintInPi(isShowFeature, !BacklogStore.getPiInfo.id)}
          {isInProgram && arr.length && arr.length > 1
            ? <ShowPlanSprint /> : null}
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
                {
                  BacklogStore.chosenVersion !== 'all' && (
                    <span className="c7n-backlog-side-tip" />
                  )
                }
              </p>
              {!isShowFeature && (
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
                  {
                    BacklogStore.chosenEpic !== 'all' && (
                      <span className="c7n-backlog-side-tip" />
                    )
                  }
                </p>
              )}
              {isShowFeature && (
                <p
                  style={{
                    marginTop: 12,
                  }}
                  role="none"
                  onClick={() => {
                    this.toggleCurrentVisible('feature');
                  }}
                >
                  特性
                  {
                    BacklogStore.chosenFeature !== 'all' && (
                      <span className="c7n-backlog-side-tip" />
                    )
                  }
                </p>
              )}
            </div>
            <Version
              store={BacklogStore}
              refresh={this.refresh}
              visible={BacklogStore.getCurrentVisible}
              issueRefresh={() => {
                this.IssueDetail.refreshIssueDetail();
              }}
            />
            {!isShowFeature && (
              <Epic
                refresh={this.refresh}
                visible={BacklogStore.getCurrentVisible}
                issueRefresh={() => {
                  this.IssueDetail.refreshIssueDetail();
                }}
              />
            )}
            {isShowFeature ? (
              <Feature
                refresh={this.refresh}
                isInProgram={isShowFeature}
                visible={BacklogStore.getCurrentVisible}
                issueRefresh={() => {
                  this.IssueDetail.refreshIssueDetail();
                }}
              />
            ) : null}
            <Spin spinning={BacklogStore.getSpinIf}>
              <div className="c7n-backlog-content">
                <SprintList />
              </div>
            </Spin>
            <CreateIssue />
            <IssueDetail
              refresh={() => this.refresh(false)}
              onRef={(ref) => {
                this.IssueDetail = ref;
              }}
            />
          </div>
        </Content>
      </>
    );
  }
}

export default (props) => (
  <Page
    service={props.isInProgram ? [
      'choerodon.code.project.cooperation.work-list.ps.backlog',
      'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.feature',
      'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint',
      'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.pi',
    ] : [
      'choerodon.code.project.cooperation.work-list.ps.backlog',
      'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint',
      'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.epic',
    ]}
    className="c7n-backlog-page"
  >
    <BacklogHome {...props} />
  </Page>
);
