import React from 'react';
import folderIcon from '@/assets/icons/folder.svg';
import documentIcon from '@/assets/icons/document.svg';
import docxIcon from '@/assets/icons/docx.svg';
import pdfIcon from '@/assets/icons/pdf.svg';
import pptxIcon from '@/assets/icons/pptx.svg';
import txtIcon from '@/assets/icons/txt.svg';
import xlsxIcon from '@/assets/icons/xlsx.svg';
import unknownIcon from '@/assets/icons/unknown.svg';

import styles from './index.less';

const FILE_ICON = {
  folder: folderIcon,
  document: documentIcon,
  docx: docxIcon,
  pptx: pptxIcon,
  pdf: pdfIcon,
  txt: txtIcon,
  xlsx: xlsxIcon,
  unknown: unknownIcon,
};

interface Props {
  type: 'folder' | 'document' | 'file'
  fileType: 'docx' | 'pptx' | 'pdf' | 'txt' | 'xlsx' | 'unknown'
  name: string,
  className?: string,
  showName?: boolean,
}

const KnowledgeName = ({
  type, fileType, name, className = '', showName = true,
}: Props) => {
  const fileIcon = FILE_ICON[type === 'file' ? fileType : (type || 'folder')] || unknownIcon;
  return (
    <div className={`${styles.knowledgeName} ${className}`}>
      <img src={fileIcon} />
      {showName && <span className={styles.name}>{name}</span>}
    </div>
  );
};

export default KnowledgeName;
