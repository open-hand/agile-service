import React, { Component } from 'react';
import {
  Input, Icon, Tooltip,
} from 'choerodon-ui';
import { Select } from 'choerodon-ui/pro';
import { observer, inject } from 'mobx-react';
import { configTheme } from '@/utils/common';
import { FlatSelect } from '@choerodon/components';
import StoryMapStore from '../../../../../stores/project/StoryMap/StoryMapStore';
import FiltersProvider from '../../../../../components/FiltersProvider';
import ClickOutSide from '../../../../../components/CommonComponent/ClickOutSide';
import IssueItem from './IssueItem';
import './SideIssueList.less';

const { Option } = FlatSelect;
@inject('HeaderStore')
@FiltersProvider(['issueStatus', 'version'])
@observer
class SideIssueList extends Component {
  state = {
    filter: '',
  }

  componentDidMount() {
    StoryMapStore.loadIssueList();
  }

  componentWillUnmount() {
    StoryMapStore.clearSideFilter();
  }

  handleCollapseClick = () => {
    StoryMapStore.setIssueListCollapse(!StoryMapStore.issueListCollapse);
  }

  handleFilter = (issue) => {
    const { filter } = this.state;
    return issue.issueNum.indexOf(filter) > -1 || issue.summary.indexOf(filter) > -1;
  }

  handleClickFilter = () => {

  }

  handleFilterChange = (e) => {
    this.setState({
      filter: e.target.value,
    });
  }

  setFilter = (field, values) => {
    StoryMapStore.handleSideFilterChange(field, values);
  }

  render() {
    const {
      issueList,
    } = StoryMapStore;
    const { filter } = this.state;
    const issues = issueList.filter(this.handleFilter);
    const { filters: { issueStatus, version: versionList }, HeaderStore } = this.props;

    return (
      <div className="c7nagile-SideIssueList" style={{ top: HeaderStore.announcementClosed ? 49 : 99 }}>
        <div className="c7nagile-SideIssueList-header">
          <div className="c7nagile-SideIssueList-input">
            <Input
              className="hidden-label"
              placeholder="按照名称搜索"
              prefix={<Icon type="search" />}
              value={filter}
              onChange={this.handleFilterChange}
            />
          </div>
        </div>
        <div style={{
          display: 'flex',
          flexWrap: 'wrap',
          alignItems: 'center',
          padding: '0 10px',
        }}
        >
          <FlatSelect
            clearButton
            multiple
            value={StoryMapStore.sideSearchVO?.advancedSearchArgs?.statusList}
            onChange={(value) => {
              this.setFilter('statusList', value);
            }}
            getPopupContainer={(trigger) => trigger.parentNode}
            placeholder="状态"
            dropdownStyle={{
              width: 180,
            }}
            maxTagCount={2}
            maxTagTextLength={3}
          >
            {issueStatus.map(({ text, value }) => <Option value={value}>{text}</Option>)}
          </FlatSelect>
          <FlatSelect
            clearButton
            multiple
            value={StoryMapStore.sideSearchVO?.advancedSearchArgs?.versionList}
            onChange={(value) => {
              this.setFilter('versionList', value);
            }}
            getPopupContainer={(trigger) => trigger.parentNode}
            placeholder="版本"
            dropdownStyle={{
              width: 180,
            }}
            maxTagCount={2}
            maxTagTextLength={3}
          >
            {versionList.concat({
              value: '0',
              text: '无版本',
            }).map(({ text, value }) => <Option value={value}>{text}</Option>)}
          </FlatSelect>
        </div>
        <div className="c7nagile-SideIssueList-content">
          {/* <Loading loading={issueListLoading} /> */}
          {issues.length > 0 ? (
            <div className="c7nagile-SideIssueList-content-list">
              {issues.map((issue) => <IssueItem issue={issue} />)}
            </div>
          ) : <div style={{ textAlign: 'center', color: 'rgba(0, 0, 0, 0.54)' }}>暂无数据</div>}
        </div>
      </div>
    );
  }
}

SideIssueList.propTypes = {

};
const SideIssueListWithClickOut = ClickOutSide(SideIssueList);
const SideIssueListContainer = observer(({ ...props }) => (StoryMapStore.sideIssueListVisible && !StoryMapStore.isFullScreen ? <SideIssueListWithClickOut {...props} /> : null));
export default SideIssueListContainer;
