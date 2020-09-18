import React, { useState, useEffect } from 'react';
import './index.less';

const PreviewReport:React.FC = () => {
  if (!document.body.classList.contains('hidden')) {
    document.body.classList.add('hidden');
  }
  useEffect(() => () => {
    document.body.classList.remove('hidden');
  }, []);
  const [value, setValue] = useState('1');
  return (
    <div>
      PreviewReport
    </div>
  );
};

export default PreviewReport;
