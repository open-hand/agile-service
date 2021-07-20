import React from 'react';
import { createPortal } from 'react-dom';
import PreviewReport from '../report-preview';

const ExportReport: React.FC = () => createPortal(
  <div id="report-export">
    <PreviewReport />
  </div>, document.body,
);
export default ExportReport;
