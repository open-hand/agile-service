import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import IssueStore from '@/stores/project/sprint/IssueStore';
import EditIssue from '@/components/EditIssue';

@inject('AppState', 'HeaderStore')
@observer
class ExpandWideCard extends Component {
  // 更新 Issue 时
  handleIssueUpdate = () => {
    const { issueRefresh } = this.props;
    issueRefresh();
  };

  // 删除Issue时
  handleIssueDelete = () => {
    const { issueRefresh } = this.props;
    issueRefresh(true);
  };

  handleIssueCopy = ({ issueId }) => {
    const { issueRefresh } = this.props;
    issueRefresh();
    IssueStore.setClickedRow({
      selectedIssue: {
        issueId,
        expand: true,
      },
    });
  }

  render() {
    const { onHideIssue } = this.props;
    return (
      <EditIssue
        {...this.props}
        visible={IssueStore.getExpand}
        issueId={IssueStore.getSelectedIssue && IssueStore.getSelectedIssue.issueId}
        onCancel={() => {
          onHideIssue();
          IssueStore.setClickedRow({
            expand: false,
            selectedIssue: {},
            checkCreateIssue: false,
          });
        }}
        onDeleteIssue={() => {
          IssueStore.setClickedRow({
            expand: false,
            selectedIssue: {},
          });
          this.handleIssueDelete();
        }}
        onUpdate={this.handleIssueUpdate}
        onIssueCopy={this.handleIssueCopy}
        onCopyAndTransformToSubIssue={this.handleIssueUpdate}
        onDeleteSubIssue={this.handleIssueDelete}
      />
    );
  }
}

export default ExpandWideCard;
