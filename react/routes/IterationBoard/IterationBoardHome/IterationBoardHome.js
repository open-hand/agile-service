import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Page, Header, Content, stores,
} from '@choerodon/boot';
import {
  Row, Col, Radio, Tooltip, Icon,
} from 'choerodon-ui';
import { loadSprint } from '../../../api/NewIssueApi';
import Assignee from '../IterationBoardComponent/Assignee';
import BurnDown from '../IterationBoardComponent/BurnDown';
import Sprint from '../IterationBoardComponent/Sprint';
import Status from '../IterationBoardComponent/Status';
import Remain from '../IterationBoardComponent/Remain';
import Priority from '../IterationBoardComponent/Priority';
import IssueType from '../IterationBoardComponent/IssueType';
import SprintDetails from '../IterationBoardComponent/SprintDetails';

import './IterationBoardHome.less';

const { AppState } = stores;
const RadioGroup = Radio.Group;
const RadioButton = Radio.Button;
@observer
class IterationBoardHome extends Component {
  constructor(props) {
    super(props);
    this.state = {
      loading: true,
      sprintId: undefined,
      sprintName: undefined,
    };
  }

  componentDidMount() {
    this.loadSprint();
  }

  loadSprint() {
    const { match } = this.props;
    const sprintId = match.params.id;
    if (!sprintId) return;
    this.setState({ loading: true });
    loadSprint(sprintId)
      .then((res) => {
        this.setState({
          loading: false,
          sprintId: res.sprintId,
          sprintName: res.sprintName,
        });
      });
  }

  renderContent() {
    const { loading, sprintId, sprintName } = this.state;
    if (!loading && !sprintId) {
      return (
        <div>
          当前项目下无冲刺
        </div>
      );
    }
    return (
      <div>
        <Row gutter={20}>
          <Col span={8}>
            <Sprint
              sprintId={sprintId}
              sprintName={sprintName}
            // link="backlog"
            />
          </Col>
          <Col span={8}>
            <Status
              sprintId={sprintId}
            // link="reporthost/pieReport/statusCode"
            />
          </Col>
          <Col span={8}>
            <Remain
              sprintId={sprintId}
            // link="backlog"
            />
          </Col>
        </Row>
        <Row gutter={20}>
          <Col span={24}>
            <BurnDown
              sprintId={sprintId}
              link="reporthost/burndownchart"
            />
          </Col>
        </Row>
        <Row gutter={20}>
          <Col span={8}>
            <IssueType
              sprintId={sprintId}
              link="reporthost/pieReport/typeCode"
            />
          </Col>

          <Col span={8}>
            <Priority
              sprintId={sprintId}
              link="reporthost/pieReport/priority"
            />
          </Col>
          <Col span={8}>
            <Assignee
              sprintId={sprintId}
              link="reporthost/pieReport/assignee"
            />
          </Col>
        </Row>
        <Row gutter={20}>
          <Col span={24}>
            <SprintDetails
              sprintId={sprintId}
              link="reporthost/sprintReport"
            />
          </Col>
        </Row>
      </div>
    );
  }

  handleModeChange = (e) => {
    const { history } = this.props;
    if (e.target.value === 'board') {
      history.push(`/agile/scrumboard?type=project&id=${AppState.currentMenuType.id}&name=${encodeURIComponent(AppState.currentMenuType.name)}&organizationId=${AppState.currentMenuType.organizationId}&orgId=${AppState.currentMenuType.organizationId}`);
    }
  }

  renderSwitchMode = () => (
    <div style={{ marginLeft: 'auto' }}>
      <RadioGroup className="c7nagile-switchRadio" value="work" style={{ marginLeft: 'auto', marginRight: 17 }} onChange={this.handleModeChange}>
        <RadioButton value="board">
          <Tooltip title="看板模式" placement="bottom">
            <Icon type="view_week" />
          </Tooltip>
        </RadioButton>
        <RadioButton value="work">
          <Tooltip title="工作台模式" placement="bottom">
            <Icon type="view_list" />
          </Tooltip>
        </RadioButton>
      </RadioGroup>
    </div>
  )

  render() {
    return (
      <Page 
        className="c7n-agile-iterationBoard"
        service={[
          'agile-service.sprint.querySprintById',
          'agile-service.iterative-worktable.querySprintInfo',
          'agile-service.iterative-worktable.queryStatusCategoryDistribute',
          'agile-service.sprint.queryNameByOptions',
          'agile-service.iterative-worktable.queryIssueTypeDistribute',
          'agile-service.iterative-worktable.queryPriorityDistribute',
          'agile-service.iterative-worktable.queryAssigneeDistribute',
          'agile-service.sprint.queryIssueByOptions',
          'agile-service.sprint.queryNonWorkdays',
          'agile-service.report.queryBurnDownCoordinate',
        ]}
      >
        <Header title="活跃冲刺" backPath={`/agile/scrumboard?type=project&id=${AppState.currentMenuType.id}&name=${encodeURIComponent(AppState.currentMenuType.name)}&organizationId=${AppState.currentMenuType.organizationId}&orgId=${AppState.currentMenuType.organizationId}`}>
          {this.renderSwitchMode()}
        </Header>
        <Content>
          {this.renderContent()}
        </Content>
      </Page>
    );
  }
}

export default IterationBoardHome;
