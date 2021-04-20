import React from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, Menu, Table, Tooltip, Modal,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  omit,
} from 'lodash';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import TableDropMenu from '@/common/TableDropMenu';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { publishVersionApi, versionApi } from '@/api';
import { usePublishVersionContext } from './stores';
import { openCreatePublishVersionModal } from './components/create-publish-version';
import { openPublishVersionDetail } from './components/publish-version-detail';
import openExportPublishVersionModal from './components/export';

const COLOR_MAP = {
  规划中: '#ffb100',
  已发布: '#00bfa5',
  归档: 'rgba(0, 0, 0, 0.3)',
};
const { Column } = Table;
const TooltipButton: React.FC<{ title?: string } & Omit<ButtonProps, 'title'>> = ({
  title, children, disabled, ...otherProps
}) => {
  if (title && disabled) {
    return <Tooltip title={title}><Button disabled={disabled} {...omit(otherProps, 'onClick')}>{children}</Button></Tooltip>;
  }
  return <Button {...otherProps}>{children}</Button>;
};

function PublishVersion() {
  const { prefixCls, tableDataSet } = usePublishVersionContext();
  function handleClickMenu(key: string, record: Record) {
    switch (key) {
      case 'del':
        Modal.confirm({
          title: '删除发布版本',
          children: (
            <div>
              <span>{`您确定要删除发布版本【${record.get('versionAlias') || record.get('version')}】？`}</span>
            </div>),
          onOk: () => {
            publishVersionApi.delete(record.get('id')).then(() => {
              tableDataSet.query();
            });
          },
        });
        break;

      default:
        break;
    }
  }
  function handleRefresh() {
    tableDataSet.query();
  }
  const renderName = ({ record, text }: RenderProps) => (
    <TableDropMenu
      text={record?.get('versionAlias') || record?.get('version')}
      style={{ lineHeight: '32px' }}
      menu={(
        <Menu onClick={({ key }) => handleClickMenu(key, record!)}>
          <Menu.Item key="publish">发布</Menu.Item>
          <Menu.Item key="del">删除</Menu.Item>
        </Menu>
      )}
      onClickEdit={() => openPublishVersionDetail(record?.get('id')!, handleRefresh)}
    />
  );
  const renderStatus = ({ text }: RenderProps) => (
    <p style={{ marginBottom: 0, minWidth: 60 }}>
      <span
        style={{
          color: '#fff',
          background: COLOR_MAP[text as keyof typeof COLOR_MAP],
          display: 'inline-block',
          lineHeight: '16px',
          height: '16px',
          borderRadius: '2px',
          padding: '0 2px',
          fontSize: '13px',
        }}
      >
        <div style={{ transform: 'scale(.8)' }}>{text === '归档' ? '已归档' : text}</div>
      </span>
    </p>
  );
  return (
    <Page>
      <Header>
        <TooltipButton
          icon="playlist_add"
          title="无相应权限创建发布版本"
          onClick={() => openCreatePublishVersionModal({ handleOk: handleRefresh })}
        >
          创建发布版本
        </TooltipButton>
        <Button icon="unarchive" onClick={openExportPublishVersionModal}>导出版本</Button>
      </Header>
      <Breadcrumb />
      <Content
        className={`${prefixCls}-content`}
        style={{
          overflowY: 'hidden', display: 'flex', flexDirection: 'column', paddingTop: 0,
        }}
      >
        <Table dataSet={tableDataSet}>
          <Column name="name" renderer={renderName} />
          <Column name="status" renderer={renderStatus} />
          <Column name="actualPublishDate" className="c7n-agile-table-cell" width={110} />
          <Column name="artifactId" className="c7n-agile-table-cell" width={100} />
          <Column name="groupId" className="c7n-agile-table-cell" width={120} />
          <Column name="appServiceName" className="c7n-agile-table-cell" width={120} />
          <Column name="tagName" className="c7n-agile-table-cell" width={100} />

        </Table>
      </Content>
    </Page>
  );
}
export default observer(PublishVersion);
