import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import TextArea from '@/components/TextArea';
import { Choerodon } from '@choerodon/boot';
import { injectIntl } from 'react-intl';
import { issueApi } from '@/api';
import TextEditToggle from '@/components/TextEditTogglePro';
import styles from './FieldSummary.less';

@inject('AppState')
@observer class FieldSummary extends Component {
  updateIssueField = (newValue) => {
    if (!newValue) {
      return;
    }
    const {
      store, onUpdate, reloadIssue, field, feature,
    } = this.props;
    const { fieldCode } = field;
    const issue = store.getIssue;
    const {
      issueId, objectVersionNumber, [fieldCode]: value, featureVO = {},
    } = issue;
    const { id, objectVersionNumber: featureObjNum } = featureVO || {};
    if (value !== newValue.trim()) {
      let obj = false;
      if (feature) {
        obj = {
          issueId,
          objectVersionNumber,
          featureVO: {
            id,
            issueId,
            objectVersionNumber: featureObjNum,
            [fieldCode]: newValue.trim(),
          },
        };
      } else if (newValue.trim()) {
        obj = {
          issueId,
          objectVersionNumber,
          [fieldCode]: newValue.trim(),
        };
      }
      if (obj) {
        issueApi.update(obj)
          .then((res) => {
            if (res.failed && res.code === 'error.epic.duplicate.feature.summary') {
              Choerodon.prompt('史诗下有相同的特性概要');
            }
            if (onUpdate) {
              onUpdate();
            }
            if (reloadIssue) {
              reloadIssue(issueId);
            }
          });
      }
    }
  };

  render() {
    const {
      store, field, feature, disabled,
    } = this.props;
    const { fieldCode } = field;
    const issue = store.getIssue;
    const { featureVO = {} } = issue;
    const value = feature ? featureVO[fieldCode] : issue[fieldCode];
    return (
      <div className="line-start" style={{ width: '100%', fontSize: 20, fontWeight: 500 }}>
        <TextEditToggle
          className={styles.summary}
          disabled={disabled}
          onSubmit={this.updateIssueField}
          initValue={value}
          alwaysRender={false}
          editor={() => (
            <TextArea
              autoSize
              maxLength="44"
              required
              validationRenderer={() => '请输入概要。'}
              labelLayout="float"
            />
          )}
        >
          <div style={{ wordBreak: 'break-all' }}>
            {value || '无'}
          </div>
        </TextEditToggle>
      </div>
    );
  }
}

export default withRouter(injectIntl(FieldSummary));
