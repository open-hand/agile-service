import React, { useMemo, useState } from 'react';
import {
  DataSet, Modal, Select, Table,
} from 'choerodon-ui/pro/lib';
import { ModalProps } from 'choerodon-ui/pro/lib/modal/Modal';
import classnames from 'classnames';
import { UploadButton } from '@/components/CommonComponent';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';

const { Column } = Table;
const ImportPom:React.FC = () => {
  const [groupId, setGroupId] = useState<string|undefined>();
  const ds = useMemo(() => new DataSet({
    autoQuery: true,
    paging: false,
    fields: [
      { name: 'appService', label: '应用服务' },
      { name: 'version', label: '版本名称' },
      { name: 'alias', label: '版本别名' },

    ],
  }), []);
  return (
    <div>
      <Select label="GROUPID" labelLayout={'float' as any} required style={{ width: '6.2rem' }}>
        <Select.Option value="0">11</Select.Option>
      </Select>
      <div>
        <span>
          上传pom文件
          <UploadButton />
        </span>

        <Table dataSet={ds}>
          <Column name="appService" />
          <Column name="version" />
          <Column name="alias" />

        </Table>
      </div>
    </div>
  );
};
function openImportPomModal() {
  const key = Modal.key();
  Modal.open({
    key,
    title: '导入pom文件',
    style: {
      width: MODAL_WIDTH.middle,
    },
    className: classnames('c7n-agile-export-issue-modal'),
    drawer: true,
    children: <ImportPom />,
    // footer: (okBtn: any, cancelBtn: any) => cancelBtn,
    // okText: store.exportButtonConfig?.buttonChildren ?? '导出',
    // okProps: { ...store.exportButtonConfig?.buttonProps },
    cancelText: '关闭',
    cancelProps: {
      color: 'primary',
    },
  });
}
export { openImportPomModal };
