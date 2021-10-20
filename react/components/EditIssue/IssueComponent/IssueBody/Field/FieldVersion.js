import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { Tooltip } from 'choerodon-ui/pro';
import { injectIntl } from 'react-intl';
import _, { map } from 'lodash';
import { issueApi } from '@/api';
import SelectVersion from '@/components/select/select-version';
import TextEditToggle from '@/components/TextEditTogglePro';

@inject('AppState')
@observer class FieldVersion extends Component {
  dataRef = React.createRef();

  transToArr = (arr, pro, type = 'string') => {
    if (!arr.length) {
      return type === 'string' ? '无' : [];
    } if (typeof arr[0] === 'object') {
      return type === 'string' ? _.map(arr, pro).join() : _.map(arr, pro);
    }
    return type === 'string' ? arr.join() : arr;
  };

  updateIssueVersion = (newVersion) => {
    const originVersions = this.dataRef.current;
    const {
      store, AppState, onCreateVersion,
    } = this.props;
    const issue = store.getIssue;
    const { versionIssueRelVOList = [], issueId, objectVersionNumber } = issue;
    const influenceVersions = _.filter(versionIssueRelVOList, { relationType: 'influence' }) || [];

    if (JSON.stringify(influenceVersions) !== JSON.stringify(newVersion)) {
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
            relationType: 'influence',
            projectId: AppState.currentMenuType.id,
          });
        }
      });
      const obj = {
        issueId,
        objectVersionNumber,
        versionIssueRelVOList: versionList,
        versionType: 'influence',
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
    const influenceVersions = _.filter(versionIssueRelVOList, { relationType: 'influence' }) || [];

    const field = store.getFieldByCode('influenceVersion');
    const required = field?.required || store.getRuleRequired(field);
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <Tooltip title="对于非当前版本所发现的缺陷进行版本选择">
            <span className="c7n-property">
              影响的版本
            </span>
          </Tooltip>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={disabled}
            style={{ width: '100%', maxWidth: '200px' }}
            onSubmit={this.updateIssueVersion}
            initValue={this.transToArr(influenceVersions, 'name', 'array')}
            editor={(
              <SelectVersion
                required={required}
                projectId={store.projectId}
                dataRef={this.dataRef}
                statusArr={[]}
                {
                  ...store.getOptionsData(field, map(influenceVersions, 'versionId'))
                }
              />
          )}
          >
            {
                influenceVersions.length ? (
                  <div>
                    <p className="primary" style={{ wordBreak: 'break-word' }}>
                      {_.map(influenceVersions, 'name').join(' , ')}
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

export default withRouter(injectIntl(FieldVersion));
