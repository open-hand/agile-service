import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import EditIssue from '../../../../components/EditIssue';
import ScrumBoardStore from '../../../../stores/project/scrumBoard/ScrumBoardStore';

@inject('AppState')
@observer
class IssueDetail extends Component {
  constructor(props) {
    super(props);
    this.state = {};
    this.EditIssue = React.createRef();
    ScrumBoardStore.setEditRef(this.EditIssue);
  }
  
  handleResetClicked = (data) => {
    // ScrumBoardStore.setIssue(data);
    if (ScrumBoardStore.getCurrentClickId !== data.issueId) {
      // 确认当前跳转点击
      ScrumBoardStore.resetCurrentClick(data.issueId, true);
      ScrumBoardStore.setIssue(data);
    }
  }

  render() {
    const { refresh } = this.props;
    return (
      <EditIssue
        visible={ScrumBoardStore.getClickedIssue}
        forwardedRef={this.EditIssue}
        backUrl="scrumboard"
        issueId={ScrumBoardStore.getCurrentClickId}
        onCancel={() => {
          ScrumBoardStore.resetClickedIssue();
        }}
        onDeleteIssue={() => {
          ScrumBoardStore.resetClickedIssue();
          refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
        }}
        onUpdate={() => {
          refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
        }}
        resetIssue={(parentIssueId) => {
          ScrumBoardStore.resetCurrentClick(parentIssueId);
        }}
      />
    );
  }
}

export default IssueDetail;
