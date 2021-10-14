import React from 'react';
import { autorun } from 'mobx';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Collapse } from 'choerodon-ui';
import { isEqual } from 'lodash';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import './RenderSwimLaneContext.less';
import scrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import SwimLaneHeader from './SwimLaneHeader';

const { Panel } = Collapse;
const getPanelKey = (mode, issue) => {
  const modeMap = new Map([
    ['swimlane_none', 'swimlaneContext%all'],
    ['assignee', `swimlaneContext%${issue.assigneeId || issue.type}`],
    ['participant', `swimlaneContext%${issue.participantId || issue.type}`],
    ['swimlane_epic', `swimlaneContext%${issue.epicId || issue.type}`],
    ['parent_child', `swimlaneContext%${issue.issueId || issue.type || 'other'}`],
  ]);
  return modeMap.get(mode);
};

const getDefaultExpanded = (mode, issueArr, key) => {
  let retArr = issueArr;
  if (mode === 'parent_child') {
    retArr = retArr.filter((issue) => !issue.isComplish || key === 'other');
  }
  return retArr.map((issue) => getPanelKey(mode, issue));
};
@observer
class SwimLaneContext extends React.Component {
  constructor(props) {
    super(props);
    const defaultCacheActiveKeys = localPageCacheStore.getItem(`scrumBoard.panel-${props.mode}`);
    this.state = {
      defaultActiveKey: defaultCacheActiveKeys,
      activeKey: [],
      issues: [],
    };
  }

  componentDidMount() {
    if (this.props.mode !== 'swimlane_none') {
      scrumBoardStore.bindFunction('expandOrUp', this.handleExpandOrUPPanel);
    }
    if (this.props.mode === 'parent_child' && this.props.fromEpic) {
      scrumBoardStore.bindFunction(`expandOrUp-componentDidMount-${this.props.mode}-${this.props.epicPrefix}`, () => true);
      // typeof (currentExpandStatus) === 'boolean' && this.handleExpandOrUPPanel(currentExpandStatus); 'expandOrUp-epic-store'
    }
    // isEqual(getDefaultExpanded(this.props.mode, [...this.props.parentIssueArr.values(), this.props.otherIssueWithoutParent]),)
  }

  componentWillUnmount() {
    scrumBoardStore.removeBindFunction('expandOrUp');
    scrumBoardStore.removeBindFunction(`expandOrUp-componentDidMount-${this.props.mode}-${this.props.epicPrefix}`);
  }

  autoExpandActive = autorun(() => {
    const [currentExpandStatus] = scrumBoardStore.executeBindFunction(['expand-current-status']);
    if (typeof (currentExpandStatus) === 'undefined') {
      return;
    }
    const [needActiveArr = [], componentStatus] = scrumBoardStore.executeBindFunction(['expandOrUp-epic-store', `expandOrUp-componentDidMount-${this.props.mode}-${this.props.epicPrefix}`]);
    if (currentExpandStatus && typeof (componentStatus) === 'undefined') {
      return;
    }
    const currentActiveItem = needActiveArr.find((activeItem) => activeItem.key === this.props.epicPrefix);
    if (currentActiveItem) {
      const keys = [...this.props.parentIssueArr.values()].slice(0, currentActiveItem.activeNumber).map((value) => getPanelKey(this.props.mode, value));
      this.panelOnChange(currentExpandStatus ? keys : []);
    }
  }, [scrumBoardStore.executeBindFunction(['expand-current-status'])])

  handleExpandOrUPPanel = (expandAll = true) => {
    this.panelOnChange(expandAll ? getDefaultExpanded(this.props.mode, [...this.props.parentIssueArr.values(), this.props.otherIssueWithoutParent]).slice(0, 15) : []);
  }

  static getDerivedStateFromProps(props, state) {
    const { mode } = props;
    const issues = [...props.parentIssueArr?.values(), props.otherIssueWithoutParent];
    const activeKey = getDefaultExpanded(mode, issues).slice(0, 15);
    const activeKeyFromOld = getDefaultExpanded(state.mode, state.issues).slice(0, 15);
    if (!isEqual(activeKey, activeKeyFromOld)) {
      return {
        mode,
        issues,
        activeKey: state.defaultActiveKey || activeKey,
        defaultActiveKey: undefined,
      };
    }
    return null;
  }

  getPanelItem = (key, parentIssue = null) => {
    const {
      children, mode, fromEpic, parentIssueArr,
    } = this.props;
    const panelKey = getPanelKey(mode, parentIssue);
    return (
      <Panel
        showArrow={mode !== 'swimlane_none'}
        key={panelKey}
        className={classnames('c7n-swimlaneContext-container', {
          shouldBeIndent: fromEpic,
          noStoryInEpic: fromEpic && Array.from(parentIssueArr).length === 0,
          taskNoStoryInEpicIndent: fromEpic && key === 'other',
          [mode]: true,
        })}
        header={(
          <SwimLaneHeader
            parentIssue={parentIssue}
            mode={mode}
            keyId={key}
            subIssueDataLength={parentIssue instanceof Array ? parentIssue.length : parentIssue.subIssueData.length}
          />
        )}
      >
        {children(this.keyConverter(key, mode))}
      </Panel>
    );
  };

  panelOnChange = (arr) => {
    this.setState({
      activeKey: arr,
    });
    localPageCacheStore.setItem(`scrumBoard.panel-${this.props.mode}`, arr);
  };

  keyConverter = (key, mode) => {
    const { epicPrefix } = this.props;
    const retMap = new Map([
      ['parent_child', `parent_child%${key}`],
      ['assignee', `assignee%${key}`],
      ['participant', `participant%${key}`],
      ['swimlane_none', 'swimlane_none%other'],
    ]);
    if (epicPrefix) {
      return `${epicPrefix}%${key}`;
    }
    return retMap.get(mode);
  };

  render() {
    const { parentIssueArr, otherIssueWithoutParent, mode } = this.props;
    const { activeKey } = this.state;
    return (
      <Collapse
        activeKey={activeKey}
        onChange={this.panelOnChange}
        bordered={false}
        destroyInactivePanel
      >
        {Array.from(parentIssueArr).map(([key, value]) => this.getPanelItem(key, value))}
        {otherIssueWithoutParent.length && this.getPanelItem('other', otherIssueWithoutParent, 'fromOther')}
      </Collapse>
    );
  }
}

export default SwimLaneContext;
