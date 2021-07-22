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
import styles from './FieldEpic.less';

@inject('AppState')
@observer class FieldEpic extends Component {
  ref = React.createRef();

  updateIssueEpic = async (newEpicId, done) => {
    const { store } = this.props;
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
      store.update(obj);
    }
  };

  updateIssueFeature = (newFeatureId) => {
    const { store } = this.props;
    const issue = store.getIssue;
    const { featureId = 1, issueId, objectVersionNumber } = issue;
    if (featureId !== newFeatureId) {
      const obj = {
        issueId,
        objectVersionNumber,
        featureId: newFeatureId || 0,
      };
      store.update(obj);
    }
  };

  render() {
    const {
      store, disabled, push, outside, projectId, organizationId,
    } = this.props;
    const issue = store.getIssue;
    const {
      epicColor, epicId, issueEpicName, typeCode,
      featureId, featureName,
    } = issue;
    const field = store.getFieldByCode('epic');
    const required = field?.required;
    return (
      <IsInProgram>
        {
          ({ isShowFeature, program }) => (
            <>
              {typeCode === 'story' && isShowFeature
                ? (
                  <div className="line-start mt-10">
                    <div className="c7n-property-wrapper">
                      <span className="c7n-property">
                        特性
                      </span>
                    </div>
                    <div className="c7n-value-wrapper" style={{ maxWidth: 'unset', display: 'flex', flexWrap: 'nowrap' }} ref={this.ref}>
                      <TextEditToggle
                        className={styles.feature}
                        disabled={disabled}
                        onSubmit={this.updateIssueFeature}
                        initValue={featureName ? featureId || [] : []}
                        editor={(
                          <SelectFeature
                            featureId={featureId}
                            featureName={featureName}
                            style={{
                              width: 200,
                            }}
                          />
                        )}
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
                      {featureId ? (
                        <div
                          role="none"
                          style={{
                            width: 60,
                            height: 20,
                            lineHeight: '20px',
                            fontSize: '12px',
                            textAlign: 'center',
                            cursor: 'pointer',
                            // margin: '10px 0 0 10px',
                            marginLeft: 10,
                            color: 'white',
                            background: '#5365EA',
                            borderRadius: '2px',
                          }}
                          onClick={() => {
                            push({
                              path: 'program_issue',
                              props: {
                                outside,
                                issueId: featureId,
                                disabled: true,
                                applyType: 'program',
                                projectId,
                                programId: program.id,
                                organizationId,
                              },
                            });
                          }}
                        >
                          查看详情
                        </div>
                      ) : null}
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
                    disabled={isShowFeature || disabled}
                    onSubmit={this.updateIssueEpic}
                    initValue={issueEpicName ? epicId || null : null}
                    editor={({ submit }) => <SelectEpic required={required} onChange={submit} />}
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
