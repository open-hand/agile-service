import * as React from 'react';

function SprintIcon(props: React.SVGProps<SVGSVGElement>) {
  return (
    <svg
      width={34}
      height={14}
      viewBox="0 0 27 13"
      xmlns="http://www.w3.org/2000/svg"
      {...props}
    >
      <path
        d="M2 0h23a2 2 0 012 2v11l-4.61-4.833H4.064L0 13V2a2 2 0 012-2z"
        fill="#B4B7C8"
        fillRule="evenodd"
      />
    </svg>
  );
}

export default SprintIcon;
