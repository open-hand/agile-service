/* eslint-disable react/sort-comp */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Page, Header, Content, stores,
} from '@choerodon/boot';
import {
  Row, Col, Radio, Tooltip, Icon,
} from 'choerodon-ui';
import { sprintApi } from '@/api';
import LINK_URL from '@/constants/LINK_URL';
import to, { linkUrl } from '@/utils/to';
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
    sprintApi.loadSprint(sprintId)
      .then((res) => {
        this.setState({
          sprintId: res.sprintId,
          sprintName: res.sprintName,
        });
      });
  }

  renderContent() {
    const { sprintId, sprintName } = this.state;
    if (!sprintId) {
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
              link={LINK_URL.reportBurnDown}
            />
          </Col>
        </Row>
        <Row gutter={20}>
          <Col span={8}>
            <IssueType
              sprintId={sprintId}
              link={LINK_URL.reportIssueType}
            />
          </Col>

          <Col span={8}>
            <Priority
              sprintId={sprintId}
              link={LINK_URL.reportPriority}
            />
          </Col>
          <Col span={8}>
            <Assignee
              sprintId={sprintId}
              link={LINK_URL.reportAssignee}
            />
          </Col>
        </Row>
        <Row gutter={20}>
          <Col span={24}>
            <SprintDetails
              sprintId={sprintId}
              link={LINK_URL.reportSprint}
            />
          </Col>
        </Row>
      </div>
    );
  }

  handleModeChange = (e) => {
    if (e.target.value === 'board') {
      to(LINK_URL.scrumboard);
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
          'choerodon.code.project.cooperation.iteration-plan.ps.report',
        ]}
      >
        <Header title="活跃冲刺" backPath={linkUrl(LINK_URL.scrumboard)}>
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
