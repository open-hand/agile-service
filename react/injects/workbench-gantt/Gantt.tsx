import React, { useState } from 'react';

const WorkbenchGantt: React.FC = () => {
  const [loading, setLoading] = useState(false);
  return (
    <div>
      <div>
        头部
      </div>
      <div>
        主体
      </div>
    </div>
  );
};

export default WorkbenchGantt;
