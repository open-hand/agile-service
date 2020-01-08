import React, { Component } from 'react';
import PropTypes from 'prop-types';
import {
  Input, Icon, Checkbox, Popover, Spin, Select,
} from 'choerodon-ui';
import { observer, inject } from 'mobx-react';
import StoryMapStore from '../../../../../stores/project/StoryMap/StoryMapStore';
import FiltersProvider from '../../../../../components/FiltersProvider';
import Loading from '../../../../../components/Loading';
import { configTheme } from '../../../../../common/utils';
import ClickOutSide from '../../../../../components/CommonComponent/ClickOutSide';
import IssueItem from './IssueItem';
import './SideIssueList.less';

const { Option, OptGroup } = Select;
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
    StoryMapStore.handleSideFilterChange(field, values.map(Number));
  }

  render() {
    const {
      issueList, issueListCollapse, issueListLoading,
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
          <div style={{
            fontWeight: 600, fontSize: '14px', marginRight: 20, marginLeft: 18,
          }}
          >
            搜索:
          </div>
          <Select
            {...configTheme({
              list: issueStatus, primary: true,
            })}
            allowClear
            mode="multiple"
            style={{ width: 100 }}
            onChange={this.setFilter.bind(this, 'statusList')}
            getPopupContainer={trigger => trigger.parentNode}
            placeholder="状态"
          >
            {issueStatus.map(({ text, value }) => <Option value={value}>{text}</Option>)}
          </Select>
          <Select
            {...configTheme({
              list: versionList.concat({
                value: '0',
                text: '无版本',
              }),
              primary: true,
            })}
            allowClear
            mode="multiple"
            style={{ width: 100 }}
            onChange={this.setFilter.bind(this, 'versionList')}
            getPopupContainer={trigger => trigger.parentNode}
            placeholder="版本"
          >
            {versionList.concat({
              value: '0',
              text: '无版本',
            }).map(({ text, value }) => <Option value={value}>{text}</Option>)}
          </Select>

        </div>
        <div className="c7nagile-SideIssueList-content">
          {/* <Loading loading={issueListLoading} /> */}
          <div className="c7nagile-SideIssueList-content-pi">
            {/* <span>{activePi.piCode}</span> */}
            {/* <Icon type={issueListCollapse ? 'expand_more' : 'expand_less'} onClick={this.handleCollapseClick} /> */}
          </div>
          {issues.length > 0 ? (
            <div className="c7nagile-SideIssueList-content-list">
              {issues.map(issue => <IssueItem issue={issue} />)}
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
