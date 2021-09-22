import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import { issueApi } from '@/api';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectMultiServiceTag from '@/components/select/select-multi-service-tag';

@inject('AppState')
@observer class FieldTag extends Component {
  updateIssueTag = (newTags) => {
    const { store } = this.props;
    const issue = store.getIssue;
    const { issueId, tags, objectVersionNumber } = issue;

    const obj = {
      issueId,
      objectVersionNumber,
      tags: newTags || [],
    };
    if (typeof (obj.tags[0]) === 'string') { //  为字符串 则为blur触发， 后续需要更换此处
      return;
    }
    // if(tags.map((i) => `${i.appServiceCode}:${i.tagName}`).join('，')===)
    store.update(obj);
  };

  render() {
    const { store, disabled, applyType } = this.props;
    const issue = store.getIssue;
    const {
      tags,
    } = issue;
    const field = store.getFieldByCode('assignee');
    const required = field?.required;
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            Tag
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ display: 'flex', flexWrap: 'nowrap' }}>
          <TextEditToggle
            disabled={disabled}
            alwaysRender={false}
            submitTrigger={['change']}
            onSubmit={this.updateIssueTag}
            initValue={tags}
            editor={({ submit, hideEditor }) => (
              <SelectMultiServiceTag onChange={submit} mode={applyType === 'program' ? 'program' : undefined} multiple onPopupHidden={(hidden) => hidden && hideEditor()} />
            )}
          >
            {
              tags && tags.length > 0 ? tags.map((i) => `${i.appServiceCode}:${i.tagName}`).join('，')
                : (
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

export default withRouter(injectIntl(FieldTag));
