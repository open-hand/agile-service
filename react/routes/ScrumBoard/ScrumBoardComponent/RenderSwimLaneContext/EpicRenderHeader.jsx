import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Collapse } from 'choerodon-ui';
import { isEqual } from 'lodash';
import './RenderSwimLaneContext.less';

import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import scrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import SwimLaneHeader from './SwimLaneHeader';

const { Panel } = Collapse;

const getDefaultExpanded = (issueArr) => [...issueArr.map((issue) => `swimlane_epic%${issue.epicId}`), 'swimlane_epic%other'];
@observer
class EpicRenderHeader extends Component {
  constructor(props) {
    super(props);
    const defaultCacheActiveKeys = localPageCacheStore.getItem(`scrumBoard.panel-epic-${props.mode}`);

    this.state = {
      defaultActiveKey: defaultCacheActiveKeys,
      activeKey: [],
      issues: [],
    };
  }

  componentDidMount() {
    scrumBoardStore.bindFunction('expandOrUp-epic', this.handleExpandOrUPPanel);
  }

  componentWillUnmount() {
    scrumBoardStore.removeBindFunction('expandOrUp-epic');
  }

  handleExpandOrUPPanel = (expandAll = true) => {
    this.panelOnChange(expandAll ? getDefaultExpanded([...this.props.parentIssueArr.values(), this.props.otherIssueWithoutParent]) : []);
  }

  static getDerivedStateFromProps(props, state) {
    const issues = [...props.parentIssueArr.values(), props.otherIssueWithoutParent];
    const activeKey = getDefaultExpanded(issues);
    const activeKeyFromOld = getDefaultExpanded(state.issues);
    if (!isEqual(activeKey, activeKeyFromOld)) {
      return {
        issues,
        activeKey: state.defaultActiveKey || activeKey,
        defaultActiveKey: undefined,
      };
    }
    return null;
  }

  getPanelKey = (key) => {
    if (key === 'other') {
      return 'swimlane_epic%other';
    }
    return `swimlane_epic%${key}`;
  };

  getPanelItem = (key, parentIssue) => {
    const { activeKey } = this.state;
    const { children, mode } = this.props;
    const panelKey = this.getPanelKey(key);
    const active = activeKey.includes(panelKey);
    return (
      <Panel
        key={this.getPanelKey(key)}
        className="c7n-swimlaneContext-container"
        header={(
          <SwimLaneHeader
            parentIssue={parentIssue}
            mode={mode}
            keyId={key}
            subIssueDataLength={parentIssue.issueArrLength}
          />
        )}
      >
        {active && children(key === 'other' ? parentIssue : parentIssue.subIssueData, key === 'other' ? 'swimlane_epic%unInterconnected' : `swimlane_epic%${parentIssue.epicId}`)}
      </Panel>
    );
  };

  panelOnChange = (arr) => {
    this.setState({
      activeKey: arr,
    });
    localPageCacheStore.setItem(`scrumBoard.panel-epic-${this.props.mode}`, arr);
  };

  render() {
    const { parentIssueArr, otherIssueWithoutParent } = this.props;
    const { activeKey } = this.state;
    return (
      <Collapse
        activeKey={activeKey}
        onChange={this.panelOnChange}
        bordered={false}
        destroyInactivePanel
      >
        {Array.from(parentIssueArr).map(([key, value]) => this.getPanelItem(key, value))}
        {this.getPanelItem('other', otherIssueWithoutParent)}
      </Collapse>
    );
  }
}

export default EpicRenderHeader;
