Imports System

Public Class Form1

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim validation As Boolean = False
        Dim input_text As String = TextBox1.Text
        Dim length_input As Integer = TextBox1.Text.Length
        Dim number_list As New List(Of String)
        Dim extra As String = ""

        Label1.ResetText()
        number_list.Clear()



        If System.Text.RegularExpressions.Regex.IsMatch(input_text, "^[0-9 ]+$") And (validation = True) Then 'Regular expression to check if entered text is ONLY numbers
            If length_input Mod 3 = 0 Then 'If the length divides equally into groups of 3

                For a = 1 To (length_input \ 3)
                    number_list.Insert(0, input_text.Substring(length_input + (a * -3), 3))
                Next

            ElseIf length_input Mod 3 = 1 Then
                If length_input = 1 Then
                    number_list.Insert(0, input_text.Substring(0, 1))
                Else
                    extra = input_text.Substring(length_input - 1, 1)
                    input_text = input_text.Remove(length_input - 1, 1)
                    length_input = input_text.Length

                    For a = 1 To (length_input \ 3)
                        number_list.Insert(0, input_text.Substring(length_input + (a * -3), 3))
                    Next

                End If

            ElseIf length_input Mod 3 = 2 Then
                If length_input = 2 Then
                    number_list.Insert(0, input_text.Substring(0, 2))
                Else
                    extra = input_text.Substring(length_input - 2, 2)
                    input_text = input_text.Remove(length_input - 2, 2)
                    length_input = input_text.Length

                    For a = 1 To (length_input \ 3)
                        number_list.Insert(0, input_text.Substring(length_input + (a * -3), 3))
                    Next
                End If

            End If

        ElseIf (System.Text.RegularExpressions.Regex.IsMatch(input_text, "^[A-Za-z]+$")) And (validation = True) Then 'Regular expression to check if entered text is ONLY letters
            MessageBox.Show("Entered test is Letters")
        ElseIf validation = False Then
            MessageBox.Show("Validation Failed")

        End If

        For i As Integer = 0 To number_list.Count - 1
            Label1.Text += (number_list(i)) & vbCr
        Next

        Label2.Text = "Groups of three: " & length_input \ 3
        Label3.Text = "New Input Text: " & input_text
        Label5.Text = "Extra: " & extra
        Label4.Text = "Original Input: " & input_text & extra

    End Sub

End Class


