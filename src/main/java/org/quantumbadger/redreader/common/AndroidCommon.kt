/*******************************************************************************
 * This file is part of RedReader.
 *
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package org.quantumbadger.redreader.common

import android.annotation.SuppressLint
import android.content.Context
import android.content.pm.PackageManager
import android.os.Handler
import android.os.Looper
import android.text.Editable
import android.text.TextWatcher
import android.view.View
import android.widget.AdapterView
import android.widget.TextView
import java.io.ByteArrayInputStream
import java.security.cert.CertificateFactory

object AndroidCommon {
    @JvmField
	val UI_THREAD_HANDLER = Handler(Looper.getMainLooper())

    @JvmStatic
	fun runOnUiThread(runnable: Runnable) {
        if (General.isThisUIThread) {
            runnable.run()
        } else {
            UI_THREAD_HANDLER.post(runnable)
        }
    }

    @JvmStatic
	fun onTextChanged(
        textBox: TextView,
        action: Runnable
    ) {
        textBox.addTextChangedListener(object : TextWatcher {
            override fun beforeTextChanged(
                s: CharSequence,
                start: Int,
                count: Int,
                after: Int
            ) {
            }

            override fun onTextChanged(
                s: CharSequence,
                start: Int,
                before: Int,
                count: Int
            ) {
            }

            override fun afterTextChanged(s: Editable) {
                action.run()
            }
        })
    }

    @JvmStatic
	fun onSelectedItemChanged(
        view: AdapterView<*>,
        action: Runnable
    ) {
        view.onItemSelectedListener = object : AdapterView.OnItemSelectedListener {
            override fun onItemSelected(
                parent: AdapterView<*>?,
                view: View?,
                position: Int,
                id: Long
            ) {
                action.run()
            }

            override fun onNothingSelected(parent: AdapterView<*>?) {
                action.run()
            }
        }
    }

	@Suppress("DEPRECATION")
	@SuppressLint("PackageManagerGetSignatures")
	fun getPackageInfo(context: Context): PackageInfo {

		val name = context.packageName
		val pInfo = context.packageManager.getPackageInfo(name, PackageManager.GET_SIGNATURES)

		return PackageInfo(
			packageName = name,
			versionCode = pInfo.versionCode,
			versionName = pInfo.versionName,
			ids = pInfo.signatures.map {
				CertificateFactory.getInstance("X509")
					.generateCertificate(ByteArrayInputStream(it.toByteArray())).encoded
			}
		)
	}

	class PackageInfo(
		val packageName: String,
		val versionCode: Int,
		val versionName: String,
		val ids: List<ByteArray>
	)
}
