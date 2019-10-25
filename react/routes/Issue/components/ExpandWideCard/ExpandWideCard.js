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

  /**
   * 重置问题点击,解决问题跳转后可再次点击
   */
  handleResetIssue = (data) => {
    const { issueId } = data;
    const { dataSet } = this.props;
    if (issueId.toString() !== dataSet.current.get('issueId')) {
      dataSet.unSelect(dataSet.current);
      IssueStore.setSelectedIssue(data);
    }
    // const record = dataSet.find(this.findCurrentIndex); unSelect
    // console.log('result', record);
    // dataSet.locate(1200);
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
        onCurrentClicked={this.handleResetIssue}
        onUpdate={this.handleIssueUpdate}
        onIssueCopy={this.handleIssueCopy}
        onCopyAndTransformToSubIssue={this.handleIssueUpdate}
        onDeleteSubIssue={this.handleIssueDelete}
      />
    );
  }
}

export default ExpandWideCard;
