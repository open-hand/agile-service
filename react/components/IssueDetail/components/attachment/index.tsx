import React from 'react';
import { Modal } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { stores } from '@choerodon/boot';
import Upload from '@/components/Upload';
import SingleFileUpload from '@/components/SingleFileUpload';
import validateFile from '@/utils/File';
import Section from '../section';
import { useDetailContext } from '../../context';

const { AppState } = stores;
interface FileItem {
  uid: number,
  name: string,
  url: string,
  userId: number,
}
const Attachment: React.FC = () => {
  const {
    store,
  } = useDetailContext();
  const { issueAttachmentVOList = [] } = store.issue;
  const readonly = true;
  const fileList: FileItem[] = issueAttachmentVOList.map((attachment) => ({
    uid: attachment.id,
    name: attachment.fileName,
    url: attachment.url,
    userId: attachment.createdBy,
  }));
  const handleRemove = (item: FileItem) => {
    Modal.confirm({
      title: '确认删除此附件？',
      onOk: () => store.deleteAttachment(item.uid),
    });
  };
  return (
    <Section
      title="附件"
      border
      buttons={!readonly ? (
        <Upload onUpload={(files) => {
          if (validateFile(files)) {
            const formData = new FormData();
            Array.prototype.forEach.call(files, (file: File) => {
              formData.append('file', file);
            });
            store.uploadAttachment(formData);
          }
        }}
        />
      ) : ''}
    >
      <div style={{ display: 'flex', flexWrap: 'wrap' }}>
        {
          fileList && fileList.length > 0 && fileList.map((item) => (
            <SingleFileUpload
              key={item.uid}
              url={item.url}
              fileName={item.name}
              onDeleteFile={() => { handleRemove(item); }}
              hasDeletePermission={!readonly && AppState.userInfo.id === item.userId}
            />
          ))
        }
      </div>
    </Section>
  );
};

export default observer(Attachment);
