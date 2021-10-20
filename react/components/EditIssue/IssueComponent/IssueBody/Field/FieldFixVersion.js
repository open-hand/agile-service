import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import _, { map } from 'lodash';
import { issueApi } from '@/api';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectVersion from '@/components/select/select-version';

@inject('AppState')
@observer class FieldFixVersion extends Component {
  dataRef = React.createRef();

  transToArr = (arr, pro, type = 'string') => {
    if (!arr.length) {
      return type === 'string' ? '无' : [];
    } if (typeof arr[0] === 'object') {
      return type === 'string' ? _.map(arr, pro).join() : _.map(arr, pro);
    }
    return type === 'string' ? arr.join() : arr;
  };

  updateIssueFixVersion = (newVersion) => {
    const originVersions = this.dataRef.current;
    const {
      store, AppState, onCreateVersion,
    } = this.props;
    const issue = store.getIssue;
    const { versionIssueRelVOList = [], issueId, objectVersionNumber } = issue;
    const fixVersions = _.filter(versionIssueRelVOList, { relationType: 'fix' }) || [];

    if (JSON.stringify(fixVersions) !== JSON.stringify(newVersion)) {
      const versionList = [];
      let newSign = false;
      (newVersion || []).forEach((version) => {
        const target = _.find(originVersions, { name: version });
        if (target) {
          versionList.push(target);
        } else {
          newSign = true;
          versionList.push({
            name: version,
            relationType: 'fix',
            projectId: AppState.currentMenuType.id,
          });
        }
      });
      const obj = {
        issueId,
        objectVersionNumber,
        versionIssueRelVOList: versionList,
        versionType: 'fix',
      };
      store.update(obj)
        .then(() => {
          // 新建版本，刷新版本侧边栏
          if (newSign && onCreateVersion) {
            onCreateVersion();
          }
        });
    }
  };

  render() {
    const {
      store, disabled, saveRef,
    } = this.props;
    saveRef(this);
    const issue = store.getIssue;
    const { versionIssueRelVOList = [] } = issue;
    const fixVersionsTotal = _.filter(versionIssueRelVOList, { relationType: 'fix' }) || [];
    const fixVersionsFixed = _.filter(fixVersionsTotal, { statusCode: 'archived' }) || [];
    const fixVersions = _.filter(fixVersionsTotal, (v) => v.statusCode !== 'archived') || [];

    const field = store.getFieldByCode('fixVersion');
    const required = field?.required || store.getRuleRequired(field);
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            修复的版本
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={disabled}
            onSubmit={this.updateIssueFixVersion}
            initValue={this.transToArr(fixVersions, 'name', 'array')}
            editExtraContent={
              fixVersionsFixed.length ? (
                <div style={{ maxWidth: 170 }}>
                  <span>已归档版本</span>
                  <span>
                    {_.map(fixVersionsFixed, 'name').join(' , ')}
                  </span>
                </div>
              ) : null
            }
            editor={(
              <SelectVersion
                required={required}
                projectId={store.projectId}
                dataRef={this.dataRef}
                statusArr={['version_planning']}
                {
                  ...store.getOptionsData(field, map(fixVersions, 'versionId'))
                }
              />
            )}
          >
            {
                fixVersionsFixed.length || fixVersions.length ? (
                  <div>
                    <div style={{ color: 'var(--text-color)' }}>
                      {_.map(fixVersionsFixed, 'name').join(' , ')}
                    </div>
                    <p className="primary" style={{ wordBreak: 'break-word' }}>
                      {_.map(fixVersions, 'name').join(' , ')}
                    </p>
                  </div>
                ) : (
                  <div>
                    无
                  </div>
                )
              }
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldFixVersion));
