import React, { useMemo, useRef, useState } from 'react';
import {
  DataSet, Modal, Select, Table,
} from 'choerodon-ui/pro/lib';
import { Button } from 'choerodon-ui';
import { Choerodon } from '@choerodon/boot';
import classnames from 'classnames';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import './index.less';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';

interface IImportPomFunctionProps {
  handleOk?: (data: any) => void
}
const { Column } = Table;
const ImportPom: React.FC = () => {
  const prefixCls = 'c7n-agile-release-detail-import-pom';
  const [groupId, setGroupId] = useState<string | undefined>();
  const inputRef = useRef<HTMLInputElement>(null);
  const ds = useMemo(() => new DataSet({
    autoQuery: true,
    paging: false,
    data: [
      { appService: '应用1', version: '0.18.a', alias: undefined },
    ],
    fields: [
      { name: 'appService', label: '应用服务' },
      { name: 'version', label: '版本名称' },
      { name: 'alias', label: '版本别名' },

    ],
  }), []);
  const handleUpload = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files[0]) {
      const file = e.target.files[0];
      if (!file) {
        Choerodon.prompt('请选择文件');
        return;
      }
      const formData = new FormData();
      formData.append('file', file);
      inputRef.current?.setAttribute('value', '');
      // issueApi.import(formData).then((res) => {

      // }).catch((e) => {

      //   Choerodon.prompt('网络错误');
      // });
    }
  };
  function renderAlias({ value }: RenderProps) {
    if (!value) {
      return <i className={`${prefixCls}-table-alias-no-value`}>请输入别名</i>;
    }
    return value;
  }
  function renderAction({ }: RenderProps) {
    return <Button icon="delete_forever" />;
  }
  return (
    <div className={prefixCls}>
      <Select label="GROUPID" labelLayout={'float' as any} required style={{ width: '6.2rem' }} value={groupId} onChange={setGroupId}>
        <Select.Option value="0">11</Select.Option>
      </Select>
      <div className={`${prefixCls}-body`}>
        <div className={`${prefixCls}-upload`}>
          <span>上传pom文件</span>
          <Button disabled={!groupId} funcType="raised" size={'small' as any} type="primary" style={{ color: groupId ? 'white' : undefined }} icon="file_upload" shape="circle" onClick={() => inputRef.current?.click()} />
        </div>

        <Table dataSet={ds} queryBar={'none' as any}>
          <Column name="appService" />
          <Column name="version" editor />
          <Column name="alias" editor renderer={renderAlias} tooltip={'overflow' as any} />
          <Column name="action" renderer={renderAction} width={65} />
        </Table>
      </div>
      <input
        ref={inputRef}
        type="file"
        onChange={handleUpload}
        style={{ display: 'none' }}
      // accept="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      />
    </div>
  );
};
function openImportPomModal(props?: IImportPomFunctionProps) {
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
  });
}
export { openImportPomModal };
