import React, { useMemo } from 'react';
import {
  DataSet, Form, Modal, Select,
} from 'choerodon-ui/pro/lib';
import classnames from 'classnames';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';

const ImportPom: React.FC = () => {
  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: true,
    paging: false,
    // data: [
    //   { appService: '应用1', alias: undefined },
    // ],
    fields: [
      { name: 'appService', label: '选择应用服务', required: true },
      { name: 'tag', label: '选择tag', required: true },
      { name: 'alias', label: '版本别名' },

    ],
  }), []);

  return (
    <Form dataSet={ds}>
      <Select name="appService" />
      <Select name="tag" />
    </Form>
  );
};
function openLinkServiceModal() {
  const key = Modal.key();
  Modal.open({
    key,
    title: '关联应用版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    className: classnames('c7n-agile-export-issue-modal'),
    drawer: true,
    children: <ImportPom />,
    // footer: (okBtn: any, cancelBtn: any) => cancelBtn,
    // okText: store.exportButtonConfig?.buttonChildren ?? '导出',
    // okProps: { ...store.exportButtonConfig?.buttonProps },
    //    cancelProps: {
    //       color: 'primary',
    //     },
  });
}
export { openLinkServiceModal };
