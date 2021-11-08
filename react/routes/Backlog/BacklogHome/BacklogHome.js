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
import { HeaderButtons } from '@choerodon/master';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import SideNav from '@/components/side-nav';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import openCreateIssue from '@/components/create-issue';
import { LoadingProvider } from '@/components/Loading';
import Version from '../components/VersionComponent/Version';
import Epic from '../components/EpicComponent/Epic';
import Feature from '../components/FeatureComponent/Feature';
import IssueDetail from '../components/issue-detail';
import CreateSprint, { CreateCurrentPiSprint } from '../components/create-sprint';
import SprintList from '../components/sprint-list';
import ShowPlanSprint from '../components/show-plan-sprint';
import './BacklogHome.less';
import StatusLinkageWSHandle from '@/components/StatusLinkageWSHandle';

const createSprintKey = Modal.key();
const createCurrentPiSprintKey = Modal.key();
const { Panel } = SideNav;
@observer
class BacklogHome extends Component {
  IssueDetailRef = React.createRef();

  componentDidMount() {
    BacklogStore.resetData();
    BacklogStore.refresh(true, true, true);
    const { isShowFeature } = this.props;
    if (isShowFeature) {
      BacklogStore.loadPiInfoAndSprint();
    }
  }

  componentWillUnmount() {
    localPageCacheStore.setItem('backlogSprintPageSize', BacklogStore.getSprintPageSize);
    BacklogStore.resetData();
    BacklogStore.clearMultiSelected();
    BacklogStore.resetFilter();
    BacklogStore.setIsInProgramData({});
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
        width: MODAL_WIDTH.small,
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
        width: MODAL_WIDTH.small,
      },
      key: createCurrentPiSprintKey,
      title: '当前PI下创建冲刺',
      children: <CreateCurrentPiSprint onCreate={onCreate} sprints={sprints} pi={piInfo} />,
    });
  };

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

  openCreateIssueModal = (originFrom) => {
    const {
      chosenEpic, chosenFeature, chosenVersion, featureList, defaultTypeId, defaultSummary, defaultSprint, defaultAssignee, defaultEpicName,
    } = BacklogStore;
    const chosenFeatureItem = featureList.find((feature) => feature.issueId === chosenFeature) || {};
    openCreateIssue({
      defaultValues: {
        summary: defaultSummary,
        epicName: defaultEpicName,
        epic: chosenEpic !== 'all' && chosenEpic !== 'unset' ? chosenEpic : undefined,
        fixVersion: chosenVersion !== 'all' && chosenVersion !== 'unset' ? chosenVersion : undefined,
        sprint: defaultSprint,
      },
      originFrom: originFrom || 'Backlog',
      defaultTypeId,
      defaultAssignee,
      defaultFeature: chosenFeature !== 'all' && chosenFeature !== 'unset' ? chosenFeatureItem : undefined,
      onCreate: (res) => {
        BacklogStore.setNewIssueVisible(false);
        BacklogStore.setDefaultSummary(undefined);
        BacklogStore.setDefaultTypeId(undefined);
        BacklogStore.setDefaultSprint(undefined);
        BacklogStore.setDefaultAssignee(undefined);
        BacklogStore.setDefaultEpicName(undefined);
        // 创建issue后刷新
        if (res) {
          BacklogStore.refresh(false, false);
        }
      },
    });
  }

  render() {
    const arr = BacklogStore.getSprintData;
    const { isInProgram, isShowFeature, theme } = this.props;
    return (
      <>
        <Header title="待办事项">
          {isInProgram && arr.length && arr.length > 1
            ? <ShowPlanSprint /> : null}
          <HeaderButtons
            items={[{
              name: '创建工作项',
              icon: 'playlist_add',
              handler: this.openCreateIssueModal,
              display: true,
            }, {
              name: '创建冲刺',
              icon: 'playlist_add',
              handler: this.handleCreateSprint,
              display: !isShowFeature,
              permissions: ['choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint'],
            }, {
              name: '当前PI下创建冲刺',
              icon: 'playlist_add',
              handler: this.handleCreateCurrentPiSprint,
              display: isShowFeature,
              disabled: !BacklogStore.getPiInfo.id,
              tooltipsConfig: {
                title: !BacklogStore.getPiInfo.id ? '无活跃的PI' : undefined,
              },
              permissions: ['choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint'],
            },
            {
              display: true,
              icon: 'refresh',
              handler: () => this.refresh(),
            },
            ]}
          />
        </Header>
        <Breadcrumb />
        {/* 盖住tab下面的边框 */}
        <div style={{
          width: '100%',
          position: 'absolute',
          background: 'white',
          height: '1px',
          top: '112px',
          left: '97px',
          zIndex: 3,
        }}
        />

        <Content style={{
          display: 'flex',
          flexDirection: 'column',
          ...theme === 'theme4' ? {
            paddingTop: 45,
            // marginLeft: 0,
            // marginRight: 0,
            paddingLeft: 0,
            paddingRight: 0,
            paddingBottom: 0,
          } : {
            padding: 0, paddingTop: 4,
          },
        }}
        >
          <LoadingProvider
            loading={BacklogStore.getSpinIf}
            globalSingle
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
                    openCreateIssueModal={this.openCreateIssueModal}
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
            <div className="c7n-backlog-content">
              <SprintList openCreateIssueModal={this.openCreateIssueModal} />
            </div>
          </LoadingProvider>

        </Content>

        <IssueDetail
          refresh={() => this.refresh(false)}
          innerRef={this.IssueDetailRef}
        />
        <StatusLinkageWSHandle />
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
