import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import {
  TabPage as Page,
  Header,
  Breadcrumb,
  Content,
  useTheme,
  Permission,
} from '@choerodon/boot';
import { Button, Spin, Tooltip } from 'choerodon-ui';
import { Modal, Button as ProButton, Tooltip as ProTooltip } from 'choerodon-ui/pro';
import { C7NFormat, HeaderButtons } from '@choerodon/master';
import { isEqual } from 'lodash';
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
import CreateSprint, {
  CreateCurrentPiSprint,
} from '../components/create-sprint';
import SprintList from '../components/sprint-list';
import ShowPlanSprint from '../components/show-plan-sprint';
import './BacklogHome.less';
import StatusLinkageWSHandle from '@/components/StatusLinkageWSHandle';

const createSprintKey = Modal.key();
const createCurrentPiSprintKey = Modal.key();
const { Panel } = SideNav;
@observer
@inject('AppState')
class BacklogHome extends Component {
  constructor(props) {
    super(props);
    this.state = {
      inNewUserGuideStepThree: false,
      createBtnToolTipHidden: true,
      origin: null,
    };
  }

  IssueDetailRef = React.createRef();

  componentDidUpdate(prevProps) {
    if (!isEqual(this.state.origin, this.props.AppState.getUserWizardStatus)) {
      // eslint-disable-next-line react/no-did-update-set-state
      this.setState({
        origin: this.props.AppState.getUserWizardStatus,
      });
      if (
        prevProps.AppState.getUserWizardStatus
      && prevProps.AppState.getUserWizardStatus[2].status === 'uncompleted'
      ) {
        // eslint-disable-next-line react/no-did-update-set-state
        this.setState({
          inNewUserGuideStepThree: true,
          createBtnToolTipHidden: false,
        });
      } else {
        // eslint-disable-next-line react/no-did-update-set-state
        this.setState({
          inNewUserGuideStepThree: false,
          createBtnToolTipHidden: true,
        });
      }
    }
  }

  componentDidMount() {
    BacklogStore.resetData();
    BacklogStore.refresh(true, true, true);
    const { isShowFeature } = this.props;
    if (isShowFeature) {
      BacklogStore.loadPiInfoAndSprint();
    }
  }

  componentWillUnmount() {
    localPageCacheStore.setItem(
      'backlogSprintPageSize',
      BacklogStore.getSprintPageSize,
    );
    BacklogStore.resetData();
    BacklogStore.clearMultiSelected();
    BacklogStore.resetFilter();
    BacklogStore.setIsInProgramData({});
  }

  refresh = (...args) => {
    BacklogStore.refresh(...args);
  };

  /**
   * 创建冲刺
   */
  handleCreateSprint = async () => {
    this.setState({
      createBtnToolTipHidden: true,
    });
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
      title: <C7NFormat
        intlPrefix="agile.backlog"
        id="current.create.sprint"
      />,
      children: (
        <CreateCurrentPiSprint
          onCreate={onCreate}
          sprints={sprints}
          pi={piInfo}
        />
      ),
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
          <span>
            <C7NFormat
              intlPrefix="agile.backlog"
              id="current.create.sprint"
            />
          </span>

        </Button>
      </Tooltip>
    ) : (
      <Permission
        service={[
          'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.subprojectupdatesprint',
        ]}
      >
        <Button icon="playlist_add" onClick={this.handleCreateCurrentPiSprint}>
          <span>
            <C7NFormat
              intlPrefix="agile.backlog"
              id="current.create.sprint"
            />
          </span>

        </Button>
      </Permission>
    );
  };

  openCreateIssueModal = (originFrom) => {
    const {
      chosenEpic,
      chosenFeature,
      chosenVersion,
      featureList,
      defaultTypeId,
      defaultSummary,
      defaultSprint,
      defaultAssignee,
      defaultEpicName,
    } = BacklogStore;
    const chosenFeatureItem = featureList.find((feature) => feature.issueId === chosenFeature) || {};
    openCreateIssue({
      defaultValues: {
        summary: defaultSummary,
        epicName: defaultEpicName,
        epic:
          chosenEpic !== 'all' && chosenEpic !== 'unset'
            ? chosenEpic
            : undefined,
        fixVersion:
          chosenVersion !== 'all' && chosenVersion !== 'unset'
            ? chosenVersion
            : undefined,
        sprint: defaultSprint,
      },
      originFrom: originFrom || 'Backlog',
      defaultTypeId,
      defaultAssignee,
      defaultFeature:
        chosenFeature !== 'all' && chosenFeature !== 'unset'
          ? chosenFeatureItem
          : undefined,
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
  };

  toHelpDoc = () => {
    window.open(
      `${this.props.AppState.getUserWizardStatus[2].helpDocs[0]}`,
      '_blank',
    );
  };

  getCreatBtnTitle = () => {
    if (this.state.inNewUserGuideStepThree) {
      return (
        <div style={{ background: '#6E80F1 !important' }}>
          <div style={{ padding: 8 }}>
            冲刺是团队处理事务的一段短期迭代周期，通常用冲刺的目标来定义冲刺，每个冲刺都发生在一定的时间期限之内，有明确的开始日期和结束日期，冲刺必须短，长度在一周到一个月之间，长度一般应当保持一致，在这个时间段内，团队需要以稳定的步调完成一组与冲刺目标一致的工作。
          </div>
          <div style={{ textAlign: 'right' }}>
            <Button
              onClick={() => {
                this.setState({
                  createBtnToolTipHidden: true,
                });
              }}
              style={{ color: '#fff', background: '#7E90F1' }}
            >
              忽略
            </Button>
            <Button
              onClick={this.toHelpDoc}
              style={{ color: '#5365EA', background: '#fff' }}
            >
              查看
            </Button>
          </div>
        </div>
      );
    }
    return '';
  };

  onHiddenBeforeChange = (hidden) => {
    if (
      this.state.inNewUserGuideStepThree
      && this.state.createBtnToolTipHidden === true
      && !hidden
    ) {
      this.setState({
        createBtnToolTipHidden: hidden,
      });
    }
  };

  getHidden = () => {
    const sprintData = BacklogStore.getSprintData;
    if (sprintData.length > 1) { // >1有冲刺，有了冲刺就不显示了 (按钮本身没有tooltip 不用考虑是不是新手阶段)
      return true;
    }
    return this.state.createBtnToolTipHidden;
  }

  createSprintBtn = () => (
    <ProTooltip
      popupClassName={
          this.state.inNewUserGuideStepThree
            ? 'c7n-pro-popup-create-sprint-guide'
            : ''
        }
      hidden={this.getHidden()}
      onHiddenBeforeChange={this.onHiddenBeforeChange}
      title={this.getCreatBtnTitle}
      placement={
          this.state.inNewUserGuideStepThree ? 'bottomRight' : 'bottom'
        }
    >
      <ProButton icon="playlist_add" onClick={this.handleCreateSprint}>
        <C7NFormat
          intlPrefix="agile.common"
          id="create.issue"
        />
      </ProButton>
    </ProTooltip>
  );

  render() {
    const arr = BacklogStore.getSprintData;
    const { isInProgram, isShowFeature, theme } = this.props;
    return (
      <>
        <Header title={(
          <C7NFormat
            intlPrefix="agile.common"
            id="backlog"
          />
        )}
        >
          {isInProgram && arr.length && arr.length > 1
            ? <ShowPlanSprint /> : null}
          <HeaderButtons
            items={[{
              name: (
                <span>
                  <C7NFormat
                    intlPrefix="agile.common"
                    id="create.issue"
                  />
                </span>),
              icon: 'playlist_add',
              handler: this.openCreateIssueModal,
              display: true,
            }, {
              element: this.createSprintBtn(),
              display: !isShowFeature,
              permissions: [
                'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.work-list.backlog.projectupdatesprint',
              ],
            }, {
              name: (
                <span>
                  <C7NFormat
                    intlPrefix="agile.backlog"
                    id="current.create.sprint"
                  />
                </span>),
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
        {/* <div style={{
          width: '100%',
          position: 'absolute',
          background: 'white',
          height: '1px',
          top: '112px',
          left: '97px',
          zIndex: 3,
        }}
        /> */}

        <Content
          style={{
            display: 'flex',
            flexDirection: 'column',
            ...(theme === 'theme4'
              ? {
                paddingTop: 45,
                // marginLeft: 0,
                // marginRight: 0,
                paddingLeft: 0,
                paddingRight: 0,
                paddingBottom: 0,
              }
              : {
                padding: 0,
                paddingTop: 4,
              }),
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
            <SideNav
              onChange={this.toggleCurrentVisible}
              activeKey={BacklogStore.whichVisible}
            >
              <Panel
                key="version"
                title={(
                  <C7NFormat
                    intlPrefix="agile.common"
                    id="version"
                  />
)}
                nav={(title) => (BacklogStore.chosenVersion !== 'all'
                  ? (
                    <>
                      <span>{title}</span>
                      <span className="c7n-backlog-side-tip" />
                    </>
                  )
                  : <span>{title}</span>)}
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
                  title={(
                    <C7NFormat
                      intlPrefix="agile.common"
                      id="epic"
                    />
)}
                  noHeader
                  nav={(title) => (BacklogStore.chosenEpic !== 'all'
                    ? (
                      <>
                        <span>{title}</span>
                        <span className="c7n-backlog-side-tip" />
                      </>
                    )
                    : <span>{title}</span>)}
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
                  title={(
                    <C7NFormat
                      intlPrefix="agile.common"
                      id="feature"
                    />
                  )}
                  nav={(title) => (BacklogStore.chosenFeature !== 'all'
                    ? (
                      <>
                        <span>{title}</span>
                        <span className="c7n-backlog-side-tip" />
                      </>
                    )
                    : <span>{title}</span>)}
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
    <Page className="c7n-backlog-page">
      <BacklogHome {...props} theme={theme} />
    </Page>
  );
};
