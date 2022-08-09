import React, {Component} from 'react';
import {inject, observer} from 'mobx-react';
import {withRouter} from 'react-router-dom';
import TextArea from '@/components/TextArea';
import {injectIntl} from 'react-intl';
import TextEditToggle from '@/components/TextEditTogglePro';
import styles from './FieldSummary.less';
import {MAX_LENGTH_SUMMARY} from "@/constants/MAX_LENGTH";

@inject('AppState')
@observer class FieldSummary extends Component {
  constructor() {
    super();
    this.ref = React.createRef();
  }

  updateIssueField = (newValue) => {
    if (!newValue) {
      return;
    }
    const {
      store, field, feature,
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
        store.update(obj);
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
      <div className="line-start" style={{ fontSize: 20, fontWeight: 500 }} ref={this.ref}>
        <TextEditToggle
          className={styles.summary}
          disabled={disabled}
          onSubmit={this.updateIssueField}
          initValue={value}
          alwaysRender={false}
          editor={() => (
            <TextArea
              autoSize
              style={{
                width: this.ref.current && this.ref.current.parentElement.clientWidth - 64,
              }}
              maxLength={MAX_LENGTH_SUMMARY}
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
