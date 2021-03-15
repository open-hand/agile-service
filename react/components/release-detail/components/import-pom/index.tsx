import React, {
  useMemo, useRef, useState, useCallback, useEffect,
} from 'react';
import {
  DataSet, Modal, Select, Spin, Table, TextField, Tooltip,
} from 'choerodon-ui/pro/lib';
import { Button } from 'choerodon-ui';
import { Choerodon } from '@choerodon/boot';
import classnames from 'classnames';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import './index.less';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import SelectTeam from '@/components/select/select-team';
import { IModalProps } from '@/common/types';
import { versionApi } from '@/api';
import Loading from '@/components/Loading';

interface IImportPomFunctionProps {
  handleOk?: ((data: any) => void) | (() => Promise<any>)
  programMode?: boolean
}

const { Column } = Table;
const ImportPom: React.FC<{ modal?: IModalProps } & IImportPomFunctionProps> = ({ modal, handleOk, programMode }) => {
  const prefixCls = 'c7n-agile-release-detail-import-pom';
  const [groupId, setGroupId] = useState<string | undefined>();
  const [loading, setLoading] = useState<boolean>(false);
  const inputRef = useRef<HTMLInputElement>(null);
  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    paging: false,
    // data: [
    //   { appService: '应用1', version: '0.18.a', alias: undefined },
    // ],
    fields: [
      { name: 'artifactId', label: '应用服务' },
      { name: 'version', label: '版本名称' },
      { name: 'versionAlias', label: '版本别名' },

    ],
  }), []);
  const handleUpload = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files[0] && groupId) {
      const file = e.target.files[0];
      if (!file) {
        Choerodon.prompt('请选择文件');
        return;
      }
      const formData = new FormData();
      formData.append('file', file);
      inputRef.current?.setAttribute('value', '');
      setLoading(true);
      versionApi.importPom(formData, groupId!).then((res) => {
        ds.loadData(res);
      }).finally(() => {
        setLoading(false);
      });

      // issueApi.import(formData).then((res) => {

      // }).catch((e) => {

      //   Choerodon.prompt('网络错误');
      // });
    }
  };
  const handleSubmit = useCallback(async () => {
    if (!await ds.validate()) {
      return false;
    }
    const data = ds.toData();
    const result = handleOk && await handleOk(data);
    return typeof (result) !== 'undefined' ? result : true;
  }, [ds, handleOk]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  function renderAlias({ value }: RenderProps) {
    if (!value) {
      return <i className={`${prefixCls}-table-alias-no-value`}>请输入别名</i>;
    }
    return value;
  }
  function renderAction({ record }: RenderProps) {
    return <Button icon="delete_forever" onClick={() => ds.delete(record!)} />;
  }
  return (
    <div className={prefixCls}>
      {programMode ? <SelectTeam labelLayout={'float' as any} style={{ width: '6.2rem' }} /> : null}
      <TextField label="groupId" labelLayout={'float' as any} required style={{ width: '6.2rem' }} onChange={setGroupId} />
      <div className={`${prefixCls}-body`}>
        <div className={`${prefixCls}-upload`}>
          <span>上传pom文件</span>
          <Tooltip title={!groupId ? '请先选择groupId' : undefined}>
            <Button disabled={!groupId} funcType="raised" size={'small' as any} type="primary" style={{ color: groupId ? 'white' : undefined }} icon="file_upload" shape="circle" onClick={() => inputRef.current?.click()} />
          </Tooltip>
        </div>

        <Table dataSet={ds} queryBar={'none' as any}>
          <Column name="artifactId" />
          <Column name="version" editor />
          <Column name="versionAlias" editor renderer={renderAlias} tooltip={'overflow' as any} />
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
      <Loading loading={loading} />
    </div>
  );
};
function openImportPomModal(props: IImportPomFunctionProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '导入pom文件',
    style: {
      width: MODAL_WIDTH.middle,
    },
    className: classnames('c7n-agile-export-issue-modal'),
    drawer: true,
    children: <ImportPom {...props} />,
  });
}
export { openImportPomModal };
