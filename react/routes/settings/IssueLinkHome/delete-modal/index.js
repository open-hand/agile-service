import React, { useMemo, Fragment, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, Select, DataSet, Radio, Icon,
} from 'choerodon-ui/pro';
import { v4 as uuidV4 } from 'uuid';

import './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

function DeleteModal(props) {
  const {
    id, linkTypeId, issueCount, name,
    modal,
    handleRefresh,
  } = props;
  const formatMessage = useFormatMessage();
  // 避免lookup缓存，保证每次打开弹窗都会请求问题链接
  const frontUuid = useMemo(() => uuidV4(), []);
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
        label: formatMessage({ id: 'agile.setting.issue_link.other' }),
        lookupAxiosConfig: {
          url: `/agile/v1/projects/${id}/issue_link_types/query_all?issueLinkTypeId=${linkTypeId}`,
          method: 'POST',
          data: { contents: [], linkName: '', frontUuid },
          transformResponse: (response) => {
            try {
              const data = JSON.parse(response);
              if (data && data.list) {
                return data.list;
              }
              return data;
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
  })), [formatMessage, id, linkTypeId]);

  useEffect(() => {
    async function handleOk() {
      try {
        if ((await deleteModalDs.submit()) !== false) {
          handleRefresh();
          return true;
        }
        return false;
      } catch (e) {
        return false;
      }
    }
    modal.handleOk(handleOk);
  }, [deleteModalDs, handleRefresh, modal]);

  return (
    <>
      {
        Number(issueCount) ? (
          <>
            <div>
              <div>
                删除工作项链接:
                <span style={{ margin: '0 10px', fontWeight: 500 }}>{name}</span>
              </div>
              <div style={{ display: 'flex', alignItems: 'center', marginTop: 10 }}>
                <Icon
                  style={{
                    color: '#d50000', fontSize: '16px', marginRight: 5,
                  }}
                  type="error"
                />
                当前有
                <span style={{ margin: '0 5px', color: 'red' }}>{issueCount}</span>
                个工作项使用此工作项链接
              </div>
              <div style={{ margin: '10px 0' }}>
                注意：将会从所有相关的工作项中删除此链接，相关的工作项可以选择关联到其他链接，或者不关联。
              </div>
            </div>
            <div>
              <Radio className="c7n-delete-issueLinkModal-radio" dataSet={deleteModalDs} name="type" value="A">{formatMessage({ id: 'agile.setting.issue_link.delete.only' }, { name: '' })}</Radio>
              <Radio className="c7n-delete-issueLinkModal-radio" dataSet={deleteModalDs} name="type" value="B">{formatMessage({ id: 'agile.setting.issue_link.delete.link.other' })}</Radio>
              {deleteModalDs.current && deleteModalDs.current.get('type') === 'B' ? (
                <Select
                  dataSet={deleteModalDs}
                  name="linkId"
                  className="c7n-delete-issueLinkModal-select"
                  placeholder="请选择一个新的工作项链接"
                />
              ) : null}
            </div>
          </>
        )
          : (
            <>
              确定要删除
              <span style={{ margin: '0 5px', fontWeight: 500 }}>{name}</span>
              工作项链接吗？
            </>
          )
      }
    </>
  );
}

export default observer(DeleteModal);
