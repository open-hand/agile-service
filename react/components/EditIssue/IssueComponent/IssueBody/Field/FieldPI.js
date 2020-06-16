import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Tooltip } from 'choerodon-ui';
import SelectFocusLoad from '@/components/SelectFocusLoad';
import TextEditToggle from '@/components/TextEditToggle';
import { piApi } from '@/api';

const { Text, Edit } = TextEditToggle;
@observer
class FieldPI extends Component {
  updateIssuePI = async (value, done) => {
    const {
      store, onUpdate, reloadIssue, 
    } = this.props;
    const issue = store.getIssue;
    const { issueId, activePi } = issue;
    const { id } = activePi || {};
    await piApi.addFeatures([issueId], id || 0, value || 0);
    if (onUpdate) {
      onUpdate();
    }    
    await reloadIssue(issueId);
    done();
  }


  render() {
    const { store, hasPermission, disabled } = this.props;
    const issue = store.getIssue;
    const { activePi, closePi } = issue;
    const {
      name, code, id, statusCode,
    } = activePi || {};
    return (
      <div className="line-start mt-10">
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            PI
          </span>
        </div>
        <div className="c7n-value-wrapper">
          <TextEditToggle
            disabled={(disabled) || (!hasPermission && statusCode === 'doing')}
            formKey="pi"
            onSubmit={this.updateIssuePI}
            originData={id}
          >
            <Text>
              <Tooltip
                placement="top"
                title={`该特性经历PI数${closePi.length + (id ? 1 : 0)}`}
              >
                <div>
                  {
                    !closePi.length && !id ? '无' : (
                      <div>
                        <div>
                          {closePi.map(p => `${p.code}-${p.name}`).join(' , ')}
                        </div>
                        {
                          id && (
                            <div
                              style={{
                                color: '#4d90fe',
                                fontSize: '13px',
                                lineHeight: '20px',
                                display: 'inline-block',
                                marginTop: closePi.length ? 5 : 0,
                              }}
                            >
                              {`${code}-${name}`}
                            </div>
                          )
                        }
                      </div>
                    )
                  }
                </div>
              </Tooltip>
            </Text>
            <Edit>
              <SelectFocusLoad
                allowClear
                type="all_pi"
                request={() => piApi.getByStatus(['todo', 'doing'])}
              />
            </Edit>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default FieldPI;
