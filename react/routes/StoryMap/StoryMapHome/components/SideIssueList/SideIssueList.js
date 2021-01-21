import React, { Component } from 'react';
import {
  Input, Icon, Select, Tooltip,
} from 'choerodon-ui';
import { observer, inject } from 'mobx-react';
import { configTheme } from '@/utils/common';
import StoryMapStore from '../../../../../stores/project/StoryMap/StoryMapStore';
import FiltersProvider from '../../../../../components/FiltersProvider';
import ClickOutSide from '../../../../../components/CommonComponent/ClickOutSide';
import IssueItem from './IssueItem';
import './SideIssueList.less';

const { Option } = Select;
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
        <div style={{ display: 'flex', alignItems: 'center' }}>
          <Select
            {...configTheme({
              list: issueStatus,
            })}
            allowClear
            mode="multiple"
            onChange={this.setFilter.bind(this, 'statusList')}
            getPopupContainer={(trigger) => trigger.parentNode}
            placeholder="状态"
            style={{
              marginLeft: 15,
            }}
            dropdownStyle={{
              width: 180,
            }}
          >
            {issueStatus.map(({ text, value }) => <Option value={value}>{text}</Option>)}
          </Select>
          <Select
            {...configTheme({
              list: versionList.concat({
                value: '0',
                text: '无版本',
              }),
            })}
            allowClear
            mode="multiple"
            onChange={this.setFilter.bind(this, 'versionList')}
            getPopupContainer={(trigger) => trigger.parentNode}
            placeholder="版本"
            dropdownStyle={{
              width: 180,
            }}
          >
            {versionList.concat({
              value: '0',
              text: '无版本',
            }).map(({ text, value }) => <Option value={value}><Tooltip title={text}>{text}</Tooltip></Option>)}
          </Select>

        </div>
        <div className="c7nagile-SideIssueList-line" />
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
