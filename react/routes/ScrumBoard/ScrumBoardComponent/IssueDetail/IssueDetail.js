import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import EditIssue from '@/components/EditIssue';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';

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
    ScrumBoardStore.setClickedIssue(data.issueId);
  }

  render() {
    const { refresh } = this.props;
    const issueId = ScrumBoardStore.getCurrentClickId;
    return (
      <EditIssue
        visible={issueId}
        forwardedRef={this.EditIssue}
        backUrl="scrumboard"
        issueId={issueId}
        onCancel={() => {
          ScrumBoardStore.resetClickedIssue();
        }}
        afterIssueUpdate={this.handleResetClicked}
        onDeleteIssue={() => {
          ScrumBoardStore.resetClickedIssue();
          refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
        }}
        onUpdate={() => {
          refresh(ScrumBoardStore.getBoardList.get(ScrumBoardStore.getSelectedBoard));
        }}
        resetIssue={(parentIssueId) => {
          ScrumBoardStore.setClickedIssue(parentIssueId);
        }}
      />
    );
  }
}

export default IssueDetail;
