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
          <PageTab title="模块管理" tabKey="choerodon.code.setting.issue.component" component={withRouter(ComponentIndex)} alwaysShow />
          <PageTab title="快速筛选" tabKey="choerodon.code.setting.issue.fastSearch" component={withRouter(FastSearchIndex)} alwaysShow />
          <PageTab title="问题链接" tabKey="choerodon.code.setting.issue.issueLink" component={withRouter(IssueLinkIndex)} alwaysShow />
        </PageWrap>
      </div>
    );
  }
}

export default SettingsIndex;
