import React, { useMemo, Fragment } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, Select, DataSet, Radio, Icon,
} from 'choerodon-ui/pro';
  
import './index.less';

function DeleteModal(props) {
  const {
    id, linkTypeId,
    formatMessage,
    modal,
    handleRefresh,
  } = props;

  const deleteModalDs = useMemo(() => (new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'type',
        type: 'string',
        defaultValue: 'A',
      },
      {
        name: 'linkId',
        type: 'string',
        textField: 'linkName',
        valueField: 'linkTypeId',
        label: formatMessage({ id: 'issue_link.other' }),
        lookupAxiosConfig: {
          url: `/agile/v1/projects/${id}/issue_link_types/query_all?issueLinkTypeId=${linkTypeId}`,
          method: 'POST',
          data: { contents: [], linkName: '' },
          transformResponse: (response) => {
            try {
              const data = JSON.parse(response);
              if (data && data.list) {
                return data.list;
              } else {
                return data;
              }
            } catch (error) {
              return response;
            }
          },
        },
      },
    ],
    transport: {
      create: ({ data: [data] }) => {
        let url = `/agile/v1/projects/${id}/issue_link_types/${linkTypeId}`;
        if (data.type === 'B') {
          url += `?toIssueLinkTypeId=${data.linkId}`;
        }
        return {
          url,
          method: 'delete',
          data: null,
        };
      },
    },
  })), []);

  async function handleOk() {
    try {
      if ((await deleteModalDs.submit()) !== false) {
        handleRefresh();
        return true;
      } else {
        return false;
      }
    } catch (e) {
      return false;
    }
  }
  
  modal.handleOk(handleOk);

  return (
    <Fragment>
      <div className="c7n-delete-modal-div-wrap">
        <Icon
          className="c7n-delete-modal-msg"
          type="error"
        />
        {formatMessage({ id: 'issue_link.msg' })}
      </div>  
      <Radio className="c7n-delete-modal-radio" dataSet={deleteModalDs} name="type" value="A">{formatMessage({ id: 'issue_link.delete.only' }, { name: '' })}</Radio>
      <Radio className="c7n-delete-modal-radio" dataSet={deleteModalDs} name="type" value="B">{formatMessage({ id: 'issue_link.delete.link.other' })}</Radio>
      {deleteModalDs.current && deleteModalDs.current.get('type') === 'B' ? <Select dataSet={deleteModalDs} name="linkId" className="c7n-delete-modal-select" /> : null}
    </Fragment>
  );
}
  
export default observer(DeleteModal);
