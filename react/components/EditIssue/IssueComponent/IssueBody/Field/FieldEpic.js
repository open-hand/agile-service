import React, { Component } from 'react';
import { Choerodon } from '@choerodon/boot';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { injectIntl } from 'react-intl';
import { featureApi, issueApi } from '@/api';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectEpic from '@/components/select/select-epic';
import SelectFeature from '@/components/select/select-feature';
import { IsInProgram } from '@/hooks/useIsInProgram';

@inject('AppState')
@observer class FieldEpic extends Component {
  updateIssueEpic = async (newEpicId, done) => {
    const { store, onUpdate, reloadIssue } = this.props;
    const issue = store.getIssue;
    const {
      epicId, issueId, objectVersionNumber, typeCode,
    } = issue;
    if (epicId !== newEpicId) {
      if (typeCode === 'feature' && newEpicId) {
        const hasSame = await featureApi.hasSameInEpicById(issueId, newEpicId);
        if (hasSame) {
          Choerodon.prompt('史诗下已含有同名特性');
          done();
          return;
        }
      }
      const obj = {
        issueId,
        objectVersionNumber,
        epicId: newEpicId || 0,
      };
      issueApi.update(obj)
        .then(() => {
          if (onUpdate) {
            onUpdate();
          }
          if (reloadIssue) {
            reloadIssue(issueId);
          }
        });
    }
  };

  updateIssueFeature = (newFeatureId) => {
    const { store, onUpdate, reloadIssue } = this.props;
    const issue = store.getIssue;
    const { featureId = 1, issueId, objectVersionNumber } = issue;
    if (featureId !== newFeatureId) {
      const obj = {
        issueId,
        objectVersionNumber,
        featureId: newFeatureId || 0,
      };
      issueApi.update(obj)
        .then(() => {
          if (onUpdate) {
            onUpdate();
          }
          if (reloadIssue) {
            reloadIssue(issueId);
          }
        });
    }
  };

  render() {
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const {
      epicColor, epicId, issueEpicName, typeCode,
      featureId, featureName,
    } = issue;
    return (
      <IsInProgram>
        {
          ({ isShowFeature }) => (
            <>
              {typeCode === 'story' && isShowFeature
                ? (
                  <div className="line-start mt-10">
                    <div className="c7n-property-wrapper">
                      <span className="c7n-property">
                        特性
                      </span>
                    </div>
                    <div className="c7n-value-wrapper">
                      <TextEditToggle
                        disabled={disabled}
                        onSubmit={this.updateIssueFeature}
                        initValue={featureName ? featureId || [] : []}
                        editor={<SelectFeature featureId={featureId} featureName={featureName} />}
                        submitTrigger={['change', 'blur']}
                      >
                        {featureName ? (
                          <div
                            className="primary"
                            style={{ wordBreak: 'break-word' }}
                          >
                            {featureName}
                          </div>
                        ) : (
                          <div>
                            无
                          </div>
                        )}
                      </TextEditToggle>
                    </div>
                  </div>
                ) : ''}
              <div className="line-start mt-10">
                <div className="c7n-property-wrapper">
                  <span className="c7n-property">
                    史诗
                  </span>
                </div>
                <div className="c7n-value-wrapper">
                  <TextEditToggle
                    disabled={featureName || disabled}
                    onSubmit={this.updateIssueEpic}
                    initValue={issueEpicName ? epicId || null : null}
                    editor={({ submit }) => <SelectEpic onChange={submit} />}
                  >
                    {
                issueEpicName ? (
                  <div
                    style={{
                      color: epicColor,
                      borderWidth: '1px',
                      borderStyle: 'solid',
                      borderColor: epicColor,
                      borderRadius: '2px',
                      fontSize: '13px',
                      lineHeight: '20px',
                      padding: '0 8px',
                      display: 'inline-block',
                      overflow: 'hidden',
                      textOverflow: 'ellipsis',
                    }}
                  >
                    {issueEpicName}
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
            </>
          )
        }
      </IsInProgram>

    );
  }
}

export default withRouter(injectIntl(FieldEpic));
