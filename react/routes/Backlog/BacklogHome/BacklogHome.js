import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import {
  TabPage as Page, Header, Breadcrumb, Content,
} from '@choerodon/boot';
import {
  Button, Spin, Icon, Tooltip,
} from 'choerodon-ui';
import { Modal } from 'choerodon-ui/pro';
import { Permission, stores } from '@choerodon/boot';
import IsInProgramStore from '@/stores/common/program/IsInProgramStore';
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
const { AppState } = stores;

@observer
class BacklogHome extends Component {
  componentDidMount() {
    const { BacklogStore } = this.props;
    BacklogStore.refresh();
    if (IsInProgramStore.isShowFeature) {
      IsInProgramStore.loadPiInfoAndSprint();
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
    await IsInProgramStore.loadPiInfoAndSprint();
    const artInfo = IsInProgramStore.getArtInfo;
    const onCreate = (sprint) => {
      BacklogStore.setCreatedSprint(sprint.sprintId);
      this.refresh();
    };
    const { programId, id: artId } = artInfo;
    const piInfo = IsInProgramStore.getPiInfo;
    const sprints = IsInProgramStore.getSprints;
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

  render() {
    const { BacklogStore } = this.props;
    const arr = BacklogStore.getSprintData;
    const { isInProgram, isShowFeature } = IsInProgramStore;
    const { type, id: projectId, organizationId: orgId } = AppState.currentMenuType;

    return (
      <Fragment>
        <Header title="待办事项">
          <Button
            className="leftBtn"
            funcType="flat"
            onClick={this.handleClickCBtn}
          >
            <Icon type="playlist_add icon" />
            <span>创建问题</span>
          </Button>
          {!isInProgram && (
            <Permission
              type={type}
              projectId={projectId}
              organizationId={orgId}
              service={['agile-service.sprint.createSprint']}
            >
              <Button className="leftBtn" functyp="flat" onClick={this.handleCreateSprint}>
                <Icon type="playlist_add icon" />
                创建冲刺
              </Button>
            </Permission>

          )}
          {isShowFeature && !IsInProgramStore.getPiInfo.id
            && (
              <Permission
                type={type}
                projectId={projectId}
                organizationId={orgId}
                service={['agile-service.sprint-pro.createSubProjectSprint']}
              >
                <Tooltip title="无活跃的PI">
                  <Button className="leftBtn" functyp="flat" onClick={this.handleCreateCurrentPiSprint} disabled>
                    <Icon type="playlist_add icon" />
                    当前PI下创建冲刺
                  </Button>
                </Tooltip>
              </Permission>
            )}
          {isShowFeature && IsInProgramStore.getPiInfo.id
            && (
              <Permission
                type={type}
                projectId={projectId}
                organizationId={orgId}
                service={['agile-service.sprint-pro.createSubProjectSprint']}
              >
                <Button className="leftBtn" functyp="flat" onClick={this.handleCreateCurrentPiSprint}>
                  <Icon type="playlist_add icon" />
                  当前PI下创建冲刺
                </Button>
              </Permission>
            )}
          {isInProgram && arr.length && arr.length > 1
            ? <ShowPlanSprint /> : null
          }
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
      </Fragment>
    );
  }
}

export default props => (
  <Page
    service={[
      'agile-service.issue.deleteIssue',
      'agile-service.sprint.queryByProjectId',
      'agile-service.issue.listFeature',
      'agile-service.product-version.queryVersionByProjectId',
      'agile-service.sprint.queryByProjectId',
      'agile-service.priority.queryDefaultByOrganizationId',
      'agile-service.scheme.queryIssueTypesWithStateMachineIdByProjectId',
      'agile-service.quick-filter.listByProjectId',
      'base-service.organization-project.getGroupInfoByEnableProject',
      'agile-service.issue.createIssue',
      'agile-service.field-value.queryPageFieldViewList',
      'agile-service.scheme.queryDefaultByOrganizationId', // /v1/projects/{project_id}/priority/default
      'agile-service.product-version.queryNameByOptions',
      'agile-service.issue-component.queryComponentById',
      'agile-service.scheme.queryByOrganizationIdList',
      'agile-service.sprint.queryNameByOptions',
      'agile-service.issue-label.listIssueLabel',
      'base-service.organization.pagingQueryUsersOnOrganization',
      'agile-service.issue.listEpicSelectData',
      'agile-service.sprint.queryNameByOptions',
      'agile-service.issue-link-type.listIssueLinkType',
      'agile-service.issue.queryIssueByOptionForAgile',
      'agile-service.sprint.queryCompleteMessageBySprintId',
      'agile-service.sprint.completeSprint',
      'base-service.time-zone-work-calendar-project.queryTimeZoneWorkCalendarDetail',
      'agile-service.sprint.querySprintById',
      'agile-service.sprint.startSprint',
      'agile-service.product-version.updateVersion',
    ]}
    className="c7n-backlog-page"
  >
    <BacklogHome {...props} />
  </Page>
);
