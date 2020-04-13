import React, { Component } from 'react';
import { Choerodon } from '@choerodon/boot';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import { Select } from 'choerodon-ui';
import { injectIntl } from 'react-intl';
import { checkFeatureNameById } from '@/api/FeatureApi';
import TextEditToggle from '../../../../TextEditToggle';
import { loadEpics, updateIssue } from '../../../../../api/NewIssueApi';
import { getFeaturesByEpic } from '../../../../../api/FeatureApi';
import IsInProgramStore from '../../../../../stores/common/program/IsInProgramStore';

const { Option } = Select;
const { Text, Edit } = TextEditToggle;

@inject('AppState')
@observer class FieldEpic extends Component {
  constructor(props) {
    super(props);
    this.state = {
      originEpics: [],
      originFeatures: [],
      selectLoading: true,
      newFeatureId: undefined,
    };
  }

  componentDidMount() {
    this.init();
  }

  componentWillReceiveProps() {
    this.init();
  }

  init = () => {
    loadEpics().then((res) => {
      this.setState({
        originEpics: res,
        selectLoading: false,
      });
    });
    getFeaturesByEpic().then((data) => {
      this.setState({
        originFeatures: data,
        selectLoading: false,
      });
    });
  };

  updateIssueEpic = async (newEpicId, done) => {
    // const { newEpicId } = this.state;
    const { store, onUpdate, reloadIssue } = this.props;
    const issue = store.getIssue;
    const {
      epicId, issueId, objectVersionNumber, typeCode, 
    } = issue;
    if (epicId !== newEpicId) {
      if (typeCode === 'feature' && newEpicId) {
        const hasSame = await checkFeatureNameById(issueId, newEpicId);
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
      updateIssue(obj)
        .then(() => {
          if (IsInProgramStore.isInProgram) {
            getFeaturesByEpic().then((data) => {
              this.setState({
                originFeatures: data,
              });
            });
          }
          if (onUpdate) {
            onUpdate();
          }
          if (reloadIssue) {
            reloadIssue(issueId);
          }
        });
    }
  };

  updateIssueFeature = () => {
    const { newFeatureId } = this.state;
    const { store, onUpdate, reloadIssue } = this.props;
    const issue = store.getIssue;
    const { featureId = 1, issueId, objectVersionNumber } = issue;
    if (featureId !== newFeatureId) {
      const obj = {
        issueId,
        objectVersionNumber,
        featureId: newFeatureId || 0,
      };
      updateIssue(obj)
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
    const {
      selectLoading, originEpics, originFeatures,
    } = this.state;
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const {
      epicColor, epicId, issueEpicName, typeCode,
      featureId, featureName,
    } = issue;
    return (
      <React.Fragment>
        {typeCode === 'story' && IsInProgramStore.isShowFeature
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
                  formKey="feature"
                  onSubmit={this.updateIssueFeature}
                  originData={featureId || []}
                >
                  <Text>
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
                    )
                    }
                  </Text>
                  <Edit>
                    <Select
                      getPopupContainer={() => document.getElementById('detail')}
                      allowClear
                      loading={selectLoading}
                      onChange={(value) => {
                        this.setState({
                          newFeatureId: value,
                        });
                      }}
                    >
                      {originFeatures.map(feature => <Option key={`${feature.issueId}`} value={feature.issueId}>{feature.summary}</Option>)}
                    </Select>
                  </Edit>
                </TextEditToggle>
              </div>
            </div>
          ) : ''
        }
        {!IsInProgramStore.isShowFeature
          && (
          <div className="line-start mt-10">
            <div className="c7n-property-wrapper">
              <span className="c7n-property">
                史诗
              </span>
            </div>
            <div className="c7n-value-wrapper">
              <TextEditToggle
                disabled={featureId || disabled}
                formKey="epic"
                onSubmit={this.updateIssueEpic}
                originData={epicId || []}
              >
                <Text>
                  {
                    epicId ? (
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
                </Text>
                <Edit>
                  <Select
                    getPopupContainer={() => document.getElementById('detail')}
                    allowClear
                    loading={selectLoading}
                  >
                    {originEpics.map(epic => <Option key={`${epic.issueId}`} value={epic.issueId}>{epic.epicName}</Option>)}
                  </Select>
                </Edit>
              </TextEditToggle>
            </div>
          </div>
          )
        }
      </React.Fragment>
    );
  }
}

export default withRouter(injectIntl(FieldEpic));
