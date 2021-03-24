import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  TabPage as Page, Header, Breadcrumb, Content, useTheme,
  Permission,
} from '@choerodon/boot';
import {
  Button, Spin, Tooltip,
} from 'choerodon-ui';
import { Modal } from 'choerodon-ui/pro';
import { findIndex } from 'lodash';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import SideNav from '@/components/side-nav';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
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
const { Panel } = SideNav;
@observer
class BacklogHome extends Component {
  IssueDetailRef = React.createRef();

  componentDidMount() {
    BacklogStore.resetData();
    BacklogStore.refresh();
    const { isShowFeature } = this.props;
    if (isShowFeature) {
      BacklogStore.loadPiInfoAndSprint();
    }
  }

  componentWillUnmount() {
    BacklogStore.resetData();
    BacklogStore.clearMultiSelected();
    BacklogStore.resetFilter();
  }

  refresh = (...args) => {
    BacklogStore.refresh(...args);
  }

  /**
   * 创建冲刺
   */
  handleCreateSprint = async () => {
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
    BacklogStore.setNewIssueVisible(true);
  }

  toggleCurrentVisible = (type) => {
    BacklogStore.toggleVisible(type);
    if (type === 'feature') {
      BacklogStore.clearMultiSelected();
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
    const arr = BacklogStore.getSprintData;
    const { isInProgram, isShowFeature, theme } = this.props;
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
          display: 'flex',
          flexDirection: 'column',
          ...theme === 'theme4' ? {} : {
            padding: 0, paddingTop: 4,
          },
        }}
        >
          <div
            className="c7n-backlog"
            style={{
              flex: 1,
              overflow: 'hidden',
            }}
          >
            <SideNav onChange={this.toggleCurrentVisible} activeKey={BacklogStore.whichVisible}>
              <Panel
                key="version"
                title="版本"
                nav={(title) => (BacklogStore.chosenVersion !== 'all'
                  ? (
                    <>
                      {title}
                      <span className="c7n-backlog-side-tip" />
                    </>
                  )
                  : title)}
              >
                <Version
                  store={BacklogStore}
                  refresh={this.refresh}
                  issueRefresh={() => {
                    this.IssueDetailRef.current.refreshIssueDetail();
                  }}
                />
              </Panel>
              {!isShowFeature ? (
                <Panel
                  key="epic"
                  title="史诗"
                  noHeader
                  nav={(title) => (BacklogStore.chosenEpic !== 'all'
                    ? (
                      <>
                        {title}
                        <span className="c7n-backlog-side-tip" />
                      </>
                    )
                    : title)}
                >
                  <Epic
                    refresh={this.refresh}
                    issueRefresh={() => {
                      this.IssueDetailRef.current.refreshIssueDetail();
                    }}
                  />
                </Panel>
              ) : (
                <Panel
                  key="feature"
                  title="特性"
                  nav={(title) => (BacklogStore.chosenFeature !== 'all'
                    ? (
                      <>
                        {title}
                        <span className="c7n-backlog-side-tip" />
                      </>
                    )
                    : title)}
                >
                  <Feature
                    refresh={this.refresh}
                    isInProgram={isShowFeature}
                    issueRefresh={() => {
                      this.IssueDetailRef.current.refreshIssueDetail();
                    }}
                  />
                </Panel>
              )}
            </SideNav>
            <Spin spinning={BacklogStore.getSpinIf}>
              <div className="c7n-backlog-content">
                <SprintList />
              </div>
            </Spin>
            <CreateIssue />
            <IssueDetail
              refresh={() => this.refresh(false)}
              innerRef={this.IssueDetailRef}
            />
          </div>
        </Content>
      </>
    );
  }
}

// eslint-disable-next-line import/no-anonymous-default-export
export default (props) => {
  const [theme] = useTheme();
  return (
    <Page
      className="c7n-backlog-page"
    >
      <BacklogHome {...props} theme={theme} />
    </Page>
  );
};
