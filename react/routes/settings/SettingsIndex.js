import React from 'react';
import { withRouter } from 'react-router-dom';
import { PageWrap, PageTab } from '@choerodon/boot';
import ComponentIndex from './Component/ComponentHome';
import FastSearchIndex from './FastSearch/FastSearchHome/FastSearchHome';
import IssueLinkIndex from './IssueLinkHome';
import './style/index.less';


class SettingsIndex extends React.Component {
  constructor(props) {
    super(props);
    this.state = {};
  }


  callback = (key) => {
    const { list } = this.state.breadcrumbData;
    list[list.length - 1].name = key;
  }

  render() {
    return (
      <div className="c7n-settings">
        <PageWrap noHeader={['choerodon.code.setting.issue.notification']} cache>
          <PageTab title="模块管理" tabKey="choerodon.code.project.setting.issue.ps.component" component={withRouter(ComponentIndex)} alwaysShow />
          <PageTab title="快速筛选" tabKey="choerodon.code.project.setting.issue.ps.fastsearch" component={withRouter(FastSearchIndex)} alwaysShow />
          <PageTab title="工作项链接" tabKey="choerodon.code.project.setting.issue.ps.issuelink" component={withRouter(IssueLinkIndex)} alwaysShow />
        </PageWrap>
      </div>
    );
  }
}

export default SettingsIndex;
